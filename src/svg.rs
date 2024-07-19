use std::collections::VecDeque;
use std::path::PathBuf;

use bevy::{
    asset::{Asset, Handle},
    color::Color,
    math::Vec2,
    reflect::{std_traits::ReflectDefault, Reflect},
    render::{mesh::Mesh, render_resource::AsBindGroup},
};
use bevy::log::trace;
use copyless::VecHelper;
use lyon_path::PathEvent;
use lyon_tessellation::{math::Point, FillTessellator, StrokeTessellator};
use svgtypes::ViewBox;
use usvg::{tiny_skia_path::{PathSegment, PathSegmentsIter}, TreeParsing, PaintOrder, Visibility, NodeExt, TreeTextToPath};

use crate::{loader::FileSvgError, render::tessellation, Convert};
use crate::util::paint::avg_gradient;

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
        path: impl Into<PathBuf>,
        fonts: Option<impl Into<PathBuf>>,
    ) -> Result<Svg, FileSvgError> {
        let mut svg_tree =
            usvg::Tree::from_data(&bytes, &usvg::Options::default()).map_err(|err| {
                FileSvgError {
                    error: err.into(),
                    path: format!("{}", path.into().display()),
                }
            })?;

        let mut fontdb = usvg::fontdb::Database::default();
        fontdb.load_system_fonts();
        fontdb.load_fonts_dir(fonts.map(|p| p.into()).unwrap_or("./assets".into()));
        svg_tree.convert_text(&fontdb);

        Ok(Svg::from_tree(svg_tree))
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

    pub(crate) fn from_tree(tree: usvg::Tree) -> Svg {
        let view_box = tree.view_box;
        let size = tree.size;
        let mut descriptors = Vec::new();

        // calculate offset for the abs_transform to center svg on its origin
        let size_center = Vec2::new(size.width(), size.height()) / 2.0;
        let view_box_center = Vec2::new(view_box.rect.width(), view_box.rect.height()) / 2.0;
        let offset = size_center - view_box_center;
        let offset = usvg::Transform::from_translate(offset.x, offset.y);

        let mut node_stack = tree.root.descendants().collect::<VecDeque<_>>();
        
        while let Some(node) = node_stack.pop_front() {
            match &*node.borrow() {
                usvg::NodeKind::Text(text) => {
                    trace!("text: {:?}", text.id);
                    if let Some(node) = &text.flattened {
                        for node in node.descendants() {
                            node_stack.push_front(node);
                        }
                    }
                }
                usvg::NodeKind::Path(ref path) => {
                    trace!("path: {:?}", path.id);
                    if matches!(path.visibility, Visibility::Hidden | Visibility::Collapse) {
                        continue;
                    }
                    let abs_transform = node.abs_transform().post_concat(offset);
                    let path_with_abs_transform = PathWithAbsTransform {
                        path,
                        abs_transform,
                    };

                    // TODO: doesn't seem to be set in 0.37
                    match path.paint_order {
                        PaintOrder::FillAndStroke => {
                            Self::process_fill(&mut descriptors, path_with_abs_transform);
                            Self::process_stroke(&mut descriptors, path_with_abs_transform);
                        }
                        PaintOrder::StrokeAndFill => {
                            Self::process_stroke(&mut descriptors, path_with_abs_transform);
                            Self::process_fill(&mut descriptors, path_with_abs_transform);
                        }
                    }
                }
                _ => {}
            }
        }

        return Svg {
            name: Default::default(),
            size: Vec2::new(size.width(), size.height()),
            view_box: ViewBox {
                x: view_box.rect.x() as f64,
                y: view_box.rect.y() as f64,
                w: view_box.rect.width() as f64,
                h: view_box.rect.height() as f64,
            },
            paths: descriptors,
            mesh: Default::default(),
        };
    }

    fn process_fill(descriptors: &mut Vec<PathDescriptor>, path_with_abs_transform: PathWithAbsTransform) {
        let path = path_with_abs_transform.path;
        // from resvg render logic
        if path.data.bounds().width() == 0.0 || path.data.bounds().height() == 0.0 {
            // Horizontal and vertical lines cannot be filled. Skip.
            return
        }
        let Some(fill) = &path.fill else {
            return;
        };
        let color = match &fill.paint {
            usvg::Paint::Color(c) => {
                Color::srgba_u8(c.red, c.green, c.blue, fill.opacity.to_u8())
            }
            usvg::Paint::LinearGradient(g) => {
                // TODO: implement
                // just taking the average between the first and last stop so we get something to render
                avg_gradient(&g.base)
            }
            usvg::Paint::RadialGradient(g) => {
                // TODO: implement
                // just taking the average between the first and last stop so we get something to render
                avg_gradient(&g.base)
            }
            _ => Color::NONE,
        };

        descriptors.alloc().init(PathDescriptor {
            segments: path_with_abs_transform.convert().collect(),
            color,
            draw_type: DrawType::Fill,
        });
    }

    fn process_stroke(descriptors: &mut Vec<PathDescriptor>, path_with_abs_transform: PathWithAbsTransform) {
        let path = path_with_abs_transform.path;
        let Some(stroke) = &path.stroke else {
            return
        };
        let (color, draw_type) = stroke.convert();

        descriptors.alloc().init(PathDescriptor {
            segments: path_with_abs_transform.convert().collect(),
            color,
            draw_type,
        });
    }
}

#[derive(Debug, Clone)]
pub struct PathDescriptor {
    pub segments: Vec<PathEvent>,
    pub color: Color,
    pub draw_type: DrawType,
}

#[derive(Debug, Clone)]
pub enum DrawType {
    Fill,
    Stroke(lyon_tessellation::StrokeOptions),
}

#[derive(Debug, Copy, Clone)]
struct PathWithAbsTransform<'a> {
    path: &'a usvg::Path,
    abs_transform: usvg::Transform,
}

// Taken from https://github.com/nical/lyon/blob/74e6b137fea70d71d3b537babae22c6652f8843e/examples/wgpu_svg/src/main.rs
pub(crate) struct PathConvIter<'iter> {
    iter: PathSegmentsIter<'iter>,
    abs_transform: usvg::Transform,
    prev: Point,
    first: Point,
    needs_end: bool,
    deferred: Option<PathEvent>,
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

        return_event.map(|event| {
            // Mapping for converting usvg::Transform to lyon_geom::Transform
            //
            // | sx  kx  tx | -> | m11 m21 m31 |
            // | ky  sy  ty | -> | m12 m22 m32 |
            // |  0   0   1 | -> |   0   0   1 |
            //
            // m11, m12,
            // m21, m22,
            // m31, m32,
            event
                .transformed(&lyon_geom::Transform::new(
                    self.abs_transform.sx, self.abs_transform.ky,
                    self.abs_transform.kx, self.abs_transform.sy,
                    self.abs_transform.tx, self.abs_transform.ty,
                ))
        })
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

impl<'iter> Convert<PathConvIter<'iter>> for PathWithAbsTransform<'iter> {
    fn convert(self) -> PathConvIter<'iter> {
        return PathConvIter {
            iter: self.path.data.segments(),
            abs_transform: self.abs_transform,
            first: Point::new(0.0, 0.0),
            prev: Point::new(0.0, 0.0),
            deferred: None,
            needs_end: false,
        };
    }
}

impl Convert<(Color, DrawType)> for &usvg::Stroke {
    #[inline]
    fn convert(self) -> (Color, DrawType) {
        let color = match &self.paint {
            usvg::Paint::Color(c) => Color::srgba_u8(c.red, c.green, c.blue, self.opacity.to_u8()),
            usvg::Paint::LinearGradient(g) => avg_gradient(&g.base),
            usvg::Paint::RadialGradient(g) => avg_gradient(&g.base),
            usvg::Paint::Pattern(_) => Color::NONE,
        };

        let linecap = match self.linecap {
            usvg::LineCap::Butt => lyon_tessellation::LineCap::Butt,
            usvg::LineCap::Square => lyon_tessellation::LineCap::Square,
            usvg::LineCap::Round => lyon_tessellation::LineCap::Round,
        };
        let linejoin = match self.linejoin {
            usvg::LineJoin::Miter => lyon_tessellation::LineJoin::Miter,
            usvg::LineJoin::MiterClip => lyon_tessellation::LineJoin::MiterClip,
            usvg::LineJoin::Bevel => lyon_tessellation::LineJoin::Bevel,
            usvg::LineJoin::Round => lyon_tessellation::LineJoin::Round,
        };

        let opt = lyon_tessellation::StrokeOptions::tolerance(0.01)
            .with_line_width(self.width.get() as f32)
            .with_line_cap(linecap)
            .with_line_join(linejoin);

        return (color, DrawType::Stroke(opt));
    }
}
