use bevy::{
    asset::{Asset, Handle},
    color::Color,
    log::{debug, error, trace, warn},
    math::{Rect, Vec2},
    reflect::{std_traits::ReflectDefault, Reflect},
    render::{mesh::Mesh, render_resource::AsBindGroup},
};
use copyless::VecHelper;
use lyon_path::PathEvent;
use lyon_tessellation::{math::Point, FillTessellator, StrokeTessellator};
use std::collections::VecDeque;
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;
use svgtypes::ViewBox;
use usvg::{
    tiny_skia_path::{PathSegment, PathSegmentsIter},
    PaintOrder,
};
use serde::{Deserialize, Serialize};
use crate::{loader::FileSvgError, render::tessellation, Convert};

#[derive(Debug, Default, Clone)]
pub struct Options {

    /// Target DPI.
    ///
    /// Impacts units conversion.
    ///
    /// Default: 96.0
    pub dpi: Option<f32>,

    /// A default font family.
    ///
    /// Will be used when no `font-family` attribute is set in the SVG.
    ///
    /// Default: Times New Roman
    pub font_family: Option<String>,

    /// A default font size.
    ///
    /// Will be used when no `font-size` attribute is set in the SVG.
    ///
    /// Default: 12
    pub font_size: Option<f32>,

    /// A list of languages.
    ///
    /// Will be used to resolve a `systemLanguage` conditional attribute.
    ///
    /// Format: en, en-US.
    ///
    /// Default: `[en]`
    pub languages: Option<Vec<String>>,

    /// Specifies the default shape rendering method.
    ///
    /// Will be used when an SVG element's `shape-rendering` property is set to `auto`.
    ///
    /// Default: GeometricPrecision
    pub shape_rendering: Option<usvg::ShapeRendering>,

    /// Specifies the default text rendering method.
    ///
    /// Will be used when an SVG element's `text-rendering` property is set to `auto`.
    ///
    /// Default: OptimizeLegibility
    pub text_rendering: Option<usvg::TextRendering>,

    /// Specifies the default image rendering method.
    ///
    /// Will be used when an SVG element's `image-rendering` property is set to `auto`.
    ///
    /// Default: OptimizeQuality
    pub image_rendering: Option<usvg::ImageRendering>,

    /// Default viewport size to assume if there is no `viewBox` attribute and
    /// the `width` or `height` attributes are relative.
    ///
    /// Default: `(100, 100)`
    pub default_size: Option<usvg::Size>,

    /// A database of fonts usable by text.
    ///
    /// This is a base database. If a custom `font_resolver` is specified,
    /// additional fonts can be loaded during parsing. Those will be added to a
    /// copy of this database. The full database containing all fonts referenced
    /// in a `Tree` becomes available as [`Tree::fontdb`](crate::Tree::fontdb)
    /// after parsing. If no fonts were loaded dynamically, that database will
    /// be the same as this one.
    pub fontdb: Option<Arc<usvg::fontdb::Database>>,
}


impl Options {
    fn build<'a>(self, fonts: Option<impl Into<PathBuf>>) -> usvg::Options<'a> {
        let fontdb = self.fontdb.unwrap_or_else(|| {
            let mut fontdb = usvg::fontdb::Database::default();
            fontdb.load_system_fonts();
            let font_dir = fonts.map(|p| p.into()).unwrap_or("./assets".into());
            debug!("loading fonts in {:?}", font_dir);
            fontdb.load_fonts_dir(font_dir);

            Arc::new(fontdb)
        });

        let mut options = usvg::Options {
            fontdb,
            ..Default::default()
        };
        // if let Some(resources_dir) = self.resources_dir {
        //     options.resources_dir = Some(resources_dir);
        // }
        if let Some(dpi) = self.dpi {
            options.dpi = dpi;
        }
        if let Some(font_family) = self.font_family {
            options.font_family = font_family;
        }
        if let Some(font_size) = self.font_size {
            options.font_size = font_size;
        }
        if let Some(languages) = self.languages {
            options.languages = languages;
        }
        if let Some(shape_rendering) = self.shape_rendering {
            options.shape_rendering = shape_rendering;
        }
        if let Some(text_rendering) = self.text_rendering {
            options.text_rendering = text_rendering;
        }
        if let Some(image_rendering) = self.image_rendering {
            options.image_rendering = image_rendering;
        }
        if let Some(default_size) = self.default_size {
            options.default_size = default_size;
        }
        // if let Some(image_href_resolver) = self.image_href_resolver {
        //     options.image_href_resolver = image_href_resolver;
        // }
        // if let Some(font_resolver) = self.font_resolver {
        //     options.font_resolver = font_resolver;
        // }

        options
    }
}




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
    pub fn from_bytes<'a>(
        bytes: &[u8],
        path: impl Into<PathBuf> + Copy,
        fonts: Option<impl Into<PathBuf>>,
        options: Option<Options>,
    ) -> Result<Svg, FileSvgError> {
        let svg_tree = usvg::Tree::from_data(
            &bytes,
            &options.unwrap_or_default().build(fonts),
        )
        .map_err(|err| FileSvgError {
            error: err.into(),
            path: format!("{}", path.into().display()),
        })?;

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
        let view_box = tree.root().layer_bounding_box();
        let size = tree.size();
        let mut descriptors = Vec::new();

        #[derive(Copy, Clone)]
        struct NodeContext<'a> {
            node: &'a usvg::Node,
            transform: usvg::Transform,
            is_text: bool,
        }

        let mut node_stack = tree
            .root()
            .children()
            .into_iter()
            // to make sure we are processing the svg with sibling > descendant priority we reverse it
            // and reverse the resulting descriptors before returning the final constructed svg
            .rev()
            .map(|node| NodeContext {
                node,
                transform: node.abs_transform(),
                is_text: false,
            })
            .collect::<VecDeque<_>>();

        while let Some(NodeContext { node, transform, is_text }) = node_stack.pop_front() {
            trace!("---");
            trace!("node: {}", node.id());
            match node {
                usvg::Node::Group(ref group) => {
                    let transform = transform.pre_concat(group.transform());
                    trace!("group: {}", group.id());
                    if !group.should_isolate() {
                        for node in group.children() {
                            node_stack.push_front(NodeContext {
                                node,
                                transform,
                                is_text: false,
                            });
                        }
                    } else {
                        todo!("group isolate not implemented")
                    }
                }
                usvg::Node::Text(ref text) => {
                    trace!("text: {}", text.id());
                    let bounding_box = text.abs_stroke_bounding_box();
                    let bounding_box = Rect::new(bounding_box.x(), bounding_box.y(), bounding_box.width(), bounding_box.height());
                    // TODO: Not sure why text with a bounding box in negative space of the canvas requires nudged back over
                    // maybe we are missing a transform but as noted below we can't rely on the transforms below this point as they
                    // are all identity and requires us to build them up ourself traversing the tree.
                    let transform = if /* !transform.is_identity() && */ (bounding_box.min.x < 0.0 || bounding_box.min.y < 0.0)  {
                        let offset_x = if bounding_box.min.x < 0.0 { bounding_box.min.x } else { 0.0 };
                        let offset_y = if bounding_box.min.y < 0.0 { bounding_box.min.y } else { 0.0 };
                        text.abs_transform().pre_concat(usvg::Transform::from_translate(-offset_x, -offset_y))
                    } else {
                        text.abs_transform()
                    };

                    // all transforms from here on down are identity
                    let group = text.flattened();
                    for node in group.children() {
                        node_stack.push_front(NodeContext {
                            node,
                            transform,
                            is_text: true,
                        });
                    }
                }
                usvg::Node::Path(ref path) => {
                    if !path.is_visible() {
                        trace!("path: {} - invisible", path.id());
                        continue;
                    }
                    trace!("path: {}", path.id());
                    let transform = if is_text {
                        transform
                    } else {
                        path.abs_transform()
                    };
                    trace!("{transform:?}");

                    let path_with_transform = PathWithTransform {
                        path,
                        transform,
                        is_stroke: false,
                    };

                    // inverted because we are reversing the list at the end
                    match path.paint_order() {
                        PaintOrder::FillAndStroke => {
                            Self::process_stroke(&mut descriptors, path_with_transform);
                            Self::process_fill(&mut descriptors, path_with_transform);
                        }
                        PaintOrder::StrokeAndFill => {
                            Self::process_fill(&mut descriptors, path_with_transform);
                            Self::process_stroke(&mut descriptors, path_with_transform);
                        }
                    }
                }
                usvg::Node::Image(image) => {
                    warn!("image: {} - not implemented", image.id());
                }
            }
        }

        descriptors.reverse();

        Svg {
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
        }
    }

    fn process_fill(descriptors: &mut Vec<PathDescriptor>, path_with_transform: PathWithTransform) {
        let path = path_with_transform.path;
        // from resvg render logic
        if path.data().bounds().width() == 0.0 || path.data().bounds().height() == 0.0 {
            // Horizontal and vertical lines cannot be filled. Skip.
            return;
        }
        let Some(fill) = &path.fill() else {
            return;
        };
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
            abs_transform: path_with_transform.transform,
            segments: path_with_transform.convert().collect(),
            color,
            draw_type: DrawType::Fill,
            is_stroke: false,
        });
    }

    fn process_stroke(
        descriptors: &mut Vec<PathDescriptor>,
        path_with_transform: PathWithTransform,
    ) {
        let mut path_with_transform = path_with_transform;
        let path = path_with_transform.path;
        let Some(stroke) = &path.stroke() else { return };
        let (color, draw_type) = stroke.convert();

        path_with_transform.is_stroke = true;

        descriptors.alloc().init(PathDescriptor {
            segments: path_with_transform.convert().collect(),
            abs_transform: path_with_transform.transform,
            color,
            draw_type,
            is_stroke: true,
        });
    }
}

#[derive(Debug, Clone)]
pub struct PathDescriptor {
    pub segments: Vec<PathEvent>,
    pub color: Color,
    pub draw_type: DrawType,
    pub abs_transform: usvg::Transform,
    pub is_stroke: bool,
}

#[derive(Debug, Clone)]
pub enum DrawType {
    Fill,
    Stroke(lyon_tessellation::StrokeOptions),
}

#[derive(Debug, Copy, Clone)]
struct PathWithTransform<'a> {
    path: &'a usvg::Path,
    is_stroke: bool,
    transform: usvg::Transform,
}

// Taken from https://github.com/nical/lyon/blob/74e6b137fea70d71d3b537babae22c6652f8843e/examples/wgpu_svg/src/main.rs
pub(crate) struct PathConvIter<'iter> {
    iter: PathSegmentsIter<'iter>,
    transform: usvg::Transform,
    prev: Point,
    first: Point,
    needs_end: bool,
    deferred: Option<PathEvent>,
    is_stroke: bool,
}

impl<'iter> Iterator for PathConvIter<'iter> {
    type Item = PathEvent;

    fn next(&mut self) -> Option<Self::Item> {
        if self.deferred.is_some() {
            return self.deferred.take();
        }
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
                    Some(PathEvent::End {
                        last,
                        first,
                        close: false,
                    })
                } else {
                    self.first = point.convert();
                    Some(PathEvent::Begin { at: self.first })
                }
            }
            Some(PathSegment::LineTo(point)) => {
                self.needs_end = true;
                let from = self.prev;
                self.prev = point.convert();
                Some(PathEvent::Line {
                    from,
                    to: self.prev,
                })
            }
            Some(PathSegment::CubicTo(point1, point2, point3)) => {
                self.needs_end = true;
                let from = self.prev;
                self.prev = point3.convert();
                Some(PathEvent::Cubic {
                    from,
                    ctrl1: point1.convert(),
                    ctrl2: point2.convert(),
                    to: self.prev,
                })
            }
            Some(PathSegment::QuadTo(point1, point2)) => {
                self.needs_end = true;
                let from = self.prev;
                self.prev = point2.convert();
                Some(PathEvent::Quadratic {
                    from,
                    ctrl: point1.convert(),
                    to: self.prev,
                })
            }
            Some(PathSegment::Close) => {
                self.needs_end = false;
                self.prev = self.first;
                Some(PathEvent::End {
                    last: self.prev,
                    first: self.first,
                    close: true,
                })
            }
            None => {
                if self.needs_end {
                    self.needs_end = false;
                    let last = self.prev;
                    let first = self.first;
                    Some(PathEvent::End {
                        last,
                        first,
                        close: false,
                    })
                } else {
                    None
                }
            }
        }
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

impl<'iter> Convert<PathConvIter<'iter>> for PathWithTransform<'iter> {
    fn convert(self) -> PathConvIter<'iter> {
        return PathConvIter {
            iter: self.path.data().segments(),
            transform: self.transform,
            first: Point::new(0.0, 0.0),
            prev: Point::new(0.0, 0.0),
            deferred: None,
            needs_end: false,
            is_stroke: self.is_stroke,
        };
    }
}

impl Convert<(Color, DrawType)> for &usvg::Stroke {
    #[inline]
    fn convert(self) -> (Color, DrawType) {
        let color = match self.paint() {
            usvg::Paint::Color(c) => {
                Color::srgba_u8(c.red, c.green, c.blue, self.opacity().to_u8())
            }
            // TODO: implement, take average for now
            usvg::Paint::LinearGradient(g) => crate::util::paint::avg_gradient(g.deref().deref()),
            usvg::Paint::RadialGradient(g) => crate::util::paint::avg_gradient(g.deref().deref()),
            usvg::Paint::Pattern(_) => Color::NONE,
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
            .with_line_width(self.width().get())
            .with_line_cap(linecap)
            .with_line_join(linejoin);

        return (color, DrawType::Stroke(opt));
    }
}
