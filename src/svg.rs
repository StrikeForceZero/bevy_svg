use std::collections::VecDeque;
use std::ops::Deref;
use std::path::PathBuf;

use bevy::{
    asset::{Asset, Handle},
    color::Color,
    math::{Mat4, Vec2},
    reflect::{std_traits::ReflectDefault, Reflect},
    render::{mesh::Mesh, render_resource::AsBindGroup},
    transform::components::Transform,
};
use copyless::VecHelper;
use lyon_geom::euclid::default::Transform2D;
use lyon_path::PathEvent;
use lyon_tessellation::{math::Point, FillTessellator, StrokeTessellator};
use svgtypes::ViewBox;
use thiserror::Error;
use usvg::{
    tiny_skia_path::{PathSegment, PathSegmentsIter},
};

use crate::{loader::FileSvgError, render::tessellation, Convert, resvg};

/// A loaded and deserialized SVG file.
#[derive(AsBindGroup, Reflect, Debug, Clone, Asset)]
#[reflect(Default, Debug)]
pub struct Svg {
    /// The name of the file.
    pub name: String,
    /// Size of the SVG.
    pub size: Vec2,
    #[reflect(ignore)]
    /// ViewBox of the SVG.
    pub view_box: ViewBox,
    #[reflect(ignore)]
    /// All paths that make up the SVG.
    pub paths: Vec<PathDescriptor>,
    /// The fully tessellated paths as [`Mesh`].
    pub mesh: Handle<Mesh>,
}

impl Default for Svg {
    fn default() -> Self {
        Self {
            name: Default::default(),
            size: Default::default(),
            view_box: ViewBox {
                x: 0.,
                y: 0.,
                w: 0.,
                h: 0.,
            },
            paths: Default::default(),
            mesh: Default::default(),
        }
    }
}

impl Svg {
    /// Loads an SVG from bytes
    pub fn from_bytes(
        bytes: &[u8],
        path: impl Into<PathBuf> + Copy,
        fonts: Option<impl Into<PathBuf>>,
    ) -> Result<Svg, FileSvgError> {
        let svg_tree =
            usvg::Tree::from_data(&bytes, &usvg::Options::default()).map_err(|err| {
                FileSvgError {
                    error: err.into(),
                    path: format!("{}", path.into().display()),
                }
            })?;

        let mut fontdb = usvg::fontdb::Database::default();
        fontdb.load_system_fonts();
        fontdb.load_fonts_dir(fonts.map(|p| p.into()).unwrap_or("./assets".into()));

        Svg::from_tree(svg_tree).map_err(|err| FileSvgError {
            error: err.into(),
            path: format!("{}", path.into().display()),
        })
    }

    /// Creates a bevy mesh from the SVG data.
    pub fn tessellate(&self) -> Mesh {
        let buffer = tessellation::generate_buffer(
            self,
            &mut FillTessellator::new(),
            &mut StrokeTessellator::new(),
        );
        buffer.convert()
    }

    pub(crate) fn from_tree(tree: usvg::Tree) -> Result<Svg, TreeError> {
        let view_box = tree.root().layer_bounding_box();
        let size = tree.size();
        let mut descriptors = Vec::new();
        let transform = tree.root().transform();

        // TODO: this might be wrong or unnecessary
        let Some(max_bbox) = resvg::tiny_skia::IntRect::from_xywh(
            -(view_box.width().ceil() as i32) * 2,
            -(view_box.height().ceil() as i32) * 2,
            view_box.width().ceil().abs() as u32 * 5,
            view_box.height().ceil().abs() as u32 * 5,
        ) else {
            return Err(TreeError::UnderOrOverflow)
        };
        
        let context = resvg::Context { max_bbox };

        struct NodeContext<'a> {
            node: &'a usvg::Node,
            context: resvg::Context,
            transform: usvg::Transform,
        }

        let mut node_stack = tree
            .root()
            .children()
            .into_iter()
            .map(|node| NodeContext {
                node,
                context,
                transform,
            })
            .collect::<VecDeque<_>>();

        while let Some(NodeContext { node, context, transform }) = node_stack.pop_front() {
            match node {
                usvg::Node::Group(ref group) => {
                    // https://github.com/RazrFalcon/resvg/blob/1a6922d5bfcee9e69e04dc47cb0b586f1ca64a1c/crates/resvg/src/render.rs#L56-L102
                    // This Source Code Form is subject to the terms of the Mozilla Public
                    // License, v. 2.0. If a copy of the MPL was not distributed with this
                    // file, You can obtain one at http://mozilla.org/MPL/2.0/.
                    let transform = transform.pre_concat(group.transform());
                    if !group.should_isolate() {
                        for node in group.children() {
                            node_stack.push_back(NodeContext {
                                node,
                                context,
                                transform,
                            });
                        }
                    } else {
                        
                        let Some(bbox) = group.layer_bounding_box().transform(transform) else {
                            return Err(TreeError::InvalidTransform);
                        };

                        let Some(mut ibbox) = (if group.filters().is_empty() {
                            // Convert group bbox into an integer one, expanding each side outwards by 2px
                            // to make sure that anti-aliased pixels would not be clipped.
                            usvg::tiny_skia_path::IntRect::from_xywh(
                                bbox.x().floor() as i32 - 2,
                                bbox.y().floor() as i32 - 2,
                                bbox.width().ceil() as u32 + 4,
                                bbox.height().ceil() as u32 + 4,
                            )
                        } else {
                            // The bounding box for groups with filters is special and should not be expanded by 2px,
                            // because it's already acting as a clipping region.
                            let bbox = bbox.to_int_rect();
                            // Make sure our filter region is not bigger than 4x the canvas size.
                            // This is required mainly to prevent huge filter regions that would tank the performance.
                            // It should not affect the final result in any way.
                            resvg::geom::fit_to_rect(bbox, context.max_bbox)
                        }) else { 
                            return Err(TreeError::InvalidTransform);
                        };

                        // Make sure our layer is not bigger than 4x the canvas size.
                        // This is required to prevent huge layers.
                        if group.filters().is_empty() {
                            let Some(new_ibbox) = resvg::geom::fit_to_rect(ibbox, context.max_bbox) else {
                                return Err(TreeError::UnderOrOverflow);
                            };
                            ibbox = new_ibbox;
                        }

                        let shift_ts = {
                            // Original shift.
                            let mut dx = bbox.x();
                            let mut dy = bbox.y();

                            // Account for subpixel positioned layers.
                            dx -= bbox.x() - ibbox.x() as f32;
                            dy -= bbox.y() - ibbox.y() as f32;

                            resvg::tiny_skia::Transform::from_translate(-dx, -dy)
                        };
                        
                        // TODO: handle filters
                        
                        // TODO: handle clipping
                        
                        // TODO: handle mask

                        let transform = shift_ts.pre_concat(transform);

                        for node in group.children() {
                            node_stack.push_back(NodeContext {
                                node,
                                context,
                                transform,
                            });
                        }
                    }
                }
                usvg::Node::Text(ref text) => {
                    let group = text.flattened();
                    let transform = transform.pre_concat(group.transform());
                    for node in group.children() {
                        node_stack.push_back(NodeContext {
                            node,
                            context,
                            transform,
                        });
                    }
                }
                usvg::Node::Path(ref path) => {
                    if !path.is_visible() {
                        continue
                    }
                    let abs_transform = node.abs_transform().convert();

                    if let Some(fill) = &path.fill() {
                        // from resvg render logic
                        if path.data().bounds().width() == 0.0 || path.data().bounds().height() == 0.0 {
                            // Horizontal and vertical lines cannot be filled. Skip.
                        } else {
                            let color = match fill.paint() {
                                usvg::Paint::Color(c) => {
                                    Color::srgba_u8(c.red, c.green, c.blue, fill.opacity().to_u8())
                                }
                                usvg::Paint::LinearGradient(g) => {
                                    // TODO: implement
                                    // just taking the average between the first and last stop so we get something to render
                                    crate::util::paint::avg_gradient(g.deref().deref())
                                }
                                usvg::Paint::RadialGradient(g) => {
                                    // TODO: implement
                                    // just taking the average between the first and last stop so we get something to render
                                    crate::util::paint::avg_gradient(g.deref().deref())
                                }
                                _ => Color::NONE,
                            };

                            descriptors.alloc().init(PathDescriptor {
                                segments: path.convert().collect(),
                                abs_transform,
                                color,
                                draw_type: DrawType::Fill,
                            });
                        }
                    }

                    if let Some(stroke) = &path.stroke() {
                        let (color, draw_type) = stroke.convert();

                        descriptors.alloc().init(PathDescriptor {
                            segments: path.convert().collect(),
                            abs_transform,
                            color,
                            draw_type,
                        });
                    }
                }
                _ => {}
            }
        }

        Ok(Svg {
            name: Default::default(),
            size: Vec2::new(size.width(), size.height()),
            view_box: ViewBox {
                x: view_box.x() as f64,
                y: view_box.y() as f64,
                w: view_box.width() as f64,
                h: view_box.height() as f64,
            },
            paths: descriptors,
            mesh: Default::default(),
        })
    }
}


#[derive(Error, Debug)]
pub enum TreeError {
    #[error("InvalidTransform")]
    InvalidTransform,
    #[error("InvalidBoundingBox")]
    InvalidBoundingBox,
    #[error("UnderOrOverflow")]
    UnderOrOverflow,
}

#[derive(Debug, Clone)]
pub struct PathDescriptor {
    pub segments: Vec<PathEvent>,
    pub abs_transform: Transform,
    pub color: Color,
    pub draw_type: DrawType,
}

#[derive(Debug, Clone)]
pub enum DrawType {
    Fill,
    Stroke(lyon_tessellation::StrokeOptions),
}

// Taken from https://github.com/nical/lyon/blob/74e6b137fea70d71d3b537babae22c6652f8843e/examples/wgpu_svg/src/main.rs
pub(crate) struct PathConvIter<'iter> {
    iter: PathSegmentsIter<'iter>,
    prev: Point,
    first: Point,
    needs_end: bool,
    deferred: Option<PathEvent>,
    scale: Transform2D<f32>,
}

impl<'iter> Iterator for PathConvIter<'iter> {
    type Item = PathEvent;

    fn next(&mut self) -> Option<Self::Item> {
        if self.deferred.is_some() {
            return self.deferred.take();
        }
        let mut return_event = None;
        let next = self.iter.next();
        match next {
            Some(PathSegment::MoveTo(point)) => {
                if self.needs_end {
                    let last = self.prev;
                    let first = self.first;
                    self.needs_end = false;
                    self.prev = point.convert();
                    self.deferred = Some(PathEvent::Begin { at: self.prev });
                    self.first = self.prev;
                    return_event = Some(PathEvent::End {
                        last,
                        first,
                        close: false,
                    });
                } else {
                    self.first = point.convert();
                    return_event = Some(PathEvent::Begin { at: self.first });
                }
            }
            Some(PathSegment::LineTo(point)) => {
                self.needs_end = true;
                let from = self.prev;
                self.prev = point.convert();
                return_event = Some(PathEvent::Line {
                    from,
                    to: self.prev,
                });
            }
            Some(PathSegment::CubicTo(point1, point2, point3)) => {
                self.needs_end = true;
                let from = self.prev;
                self.prev = point3.convert();
                return_event = Some(PathEvent::Cubic {
                    from,
                    ctrl1: point1.convert(),
                    ctrl2: point2.convert(),
                    to: self.prev,
                });
            }
            Some(PathSegment::QuadTo(point1, point2)) => {
                self.needs_end = true;
                let from = self.prev;
                self.prev = point2.convert();
                return_event = Some(PathEvent::Quadratic {
                    from,
                    ctrl: point1.convert(),
                    to: self.prev,
                });
            }
            Some(PathSegment::Close) => {
                self.needs_end = false;
                self.prev = self.first;
                return_event = Some(PathEvent::End {
                    last: self.prev,
                    first: self.first,
                    close: true,
                });
            }
            None => {
                if self.needs_end {
                    self.needs_end = false;
                    let last = self.prev;
                    let first = self.first;
                    return_event = Some(PathEvent::End {
                        last,
                        first,
                        close: false,
                    });
                }
            }
        }

        return return_event.map(|event| event.transformed(&self.scale));
    }
}

impl Convert<Point> for &usvg::tiny_skia_path::Point {
    #[inline]
    fn convert(self) -> Point {
        Point::new(self.x, self.y)
    }
}

impl Convert<Point> for usvg::tiny_skia_path::Point {
    #[inline]
    fn convert(self) -> Point {
        Point::new(self.x, self.y)
    }
}

impl Convert<Transform> for usvg::tiny_skia_path::Transform {
    #[inline]
    fn convert(self) -> Transform {
        Transform::from_matrix(Mat4::from_cols(
            [self.sx, self.ky, 0.0, 0.0].into(),
            [self.kx, self.sy, 0.0, 0.0].into(),
            [0.0, 0.0, 1.0, 0.0].into(),
            [self.tx, self.ty, 0.0, 1.0].into(),
        ))
    }
}

impl<'iter> Convert<PathConvIter<'iter>> for &'iter usvg::Path {
    fn convert(self) -> PathConvIter<'iter> {
        // TODO: verify abs_transform is expected
        let (scale_x, scale_y) = self.abs_transform().get_scale();
        return PathConvIter {
            iter: self.data().segments(),
            first: Point::new(0.0, 0.0),
            prev: Point::new(0.0, 0.0),
            deferred: None,
            needs_end: false,
            scale: lyon_geom::Transform::scale(scale_x, scale_y),
        };
    }
}

impl Convert<(Color, DrawType)> for &usvg::Stroke {
    #[inline]
    fn convert(self) -> (Color, DrawType) {
        let color = match self.paint() {
            usvg::Paint::Color(c) => Color::srgba_u8(c.red, c.green, c.blue, self.opacity().to_u8()),
            usvg::Paint::LinearGradient(_)
            | usvg::Paint::RadialGradient(_)
            | usvg::Paint::Pattern(_) => Color::NONE,
        };

        let linecap = match self.linecap() {
            usvg::LineCap::Butt => lyon_tessellation::LineCap::Butt,
            usvg::LineCap::Square => lyon_tessellation::LineCap::Square,
            usvg::LineCap::Round => lyon_tessellation::LineCap::Round,
        };
        let linejoin = match self.linejoin() {
            usvg::LineJoin::Miter => lyon_tessellation::LineJoin::Miter,
            usvg::LineJoin::MiterClip => lyon_tessellation::LineJoin::MiterClip,
            usvg::LineJoin::Bevel => lyon_tessellation::LineJoin::Bevel,
            usvg::LineJoin::Round => lyon_tessellation::LineJoin::Round,
        };

        let opt = lyon_tessellation::StrokeOptions::tolerance(0.01)
            .with_line_width(self.width().get() as f32)
            .with_line_cap(linecap)
            .with_line_join(linejoin);

        return (color, DrawType::Stroke(opt));
    }
}
