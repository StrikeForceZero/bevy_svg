use bevy::{
    log::{debug, error},
    math::Vec3,
    transform::components::Transform,
};
use lyon_tessellation::{BuffersBuilder, FillOptions, FillTessellator, StrokeTessellator};

use crate::{
    render::vertex_buffer::{BufferExt, VertexBuffers, VertexConstructor},
    svg::{DrawType, Svg},
};

pub(crate) fn generate_buffer(
    svg: &Svg,
    fill_tess: &mut FillTessellator,
    stroke_tess: &mut StrokeTessellator,
) -> VertexBuffers {
    debug!("Tessellating SVG: {}", svg.name);

    let mut buffers = VertexBuffers::new();

    let mut color = None;
    for path in &svg.paths {
        let mut buffer = VertexBuffers::new();

        if color.is_none() {
            color = Some(path.color);
        }

        match path.draw_type {
            DrawType::Fill => {
                if let Err(e) = fill_tess.tessellate(
                    path.segments.clone(),
                    &FillOptions::tolerance(0.001),
                    &mut BuffersBuilder::new(
                        &mut buffer,
                        VertexConstructor {
                            color: path.color,
                        },
                    ),
                ) {
                    error!("FillTessellator error: {:?}", e)
                }
            }
            DrawType::Stroke(opts) => {
                if let Err(e) = stroke_tess.tessellate(
                    path.segments.clone(),
                    &opts,
                    &mut BuffersBuilder::new(
                        &mut buffer,
                        VertexConstructor {
                            color: path.color,
                        },
                    ),
                ) {
                    error!("StrokeTessellator error: {:?}", e)
                }
            }
        }
        buffers.extend_one(buffer);
    }
    debug!("Tessellating SVG: {} ... Done", svg.name);

    buffers
}
