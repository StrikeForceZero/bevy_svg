use bevy::{
    log::{debug, error},
    math::Vec3,
    transform::components::Transform,
};
use bevy::log::info;
use lyon_tessellation::{BuffersBuilder, FillOptions, FillTessellator, StrokeTessellator};

use crate::{Convert, render::vertex_buffer::{BufferExt, VertexBuffers, VertexConstructor}, svg::{DrawType, Svg}};

pub(crate) fn generate_buffer(
    svg: &Svg,
    fill_tess: &mut FillTessellator,
    stroke_tess: &mut StrokeTessellator,
) -> VertexBuffers {
    debug!("Tessellating SVG: {}", svg.name);

    let mut buffers = VertexBuffers::new();

    let mut color = None;
    for mut path in &svg.paths {
        let mut buffer = VertexBuffers::new();

        if color.is_none() {
            color = Some(path.color);
        }

        let mut transform = path.abs_transform;
        let segments = if path.transform != path.abs_transform {
            info!("fixing transform");
            transform = path.transform.post_concat(path.abs_transform);
            // path.segments.iter().map(|s| s.transformed(&lyon_geom::Transform::from_array([
            //     path.transform.sx,
            //     path.transform.kx,
            //     path.transform.ky,
            //     path.transform.sy,
            //     path.transform.tx,
            //     path.transform.ty,
            // ]))).collect()
            path.segments.clone()
        } else {
            path.segments.clone()
        };
        match path.draw_type {
            DrawType::Fill => {
                if let Err(e) = fill_tess.tessellate(
                    segments,
                    &FillOptions::tolerance(0.001),
                    &mut BuffersBuilder::new(
                        &mut buffer,
                        VertexConstructor {
                            color: path.color,
                            transform: transform.convert(),
                        },
                    ),
                ) {
                    error!("FillTessellator error: {:?}", e)
                }
            }
            DrawType::Stroke(opts) => {
                if let Err(e) = stroke_tess.tessellate(
                    segments,
                    &opts,
                    &mut BuffersBuilder::new(
                        &mut buffer,
                        VertexConstructor {
                            color: path.color,
                            transform: transform.convert(),
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
