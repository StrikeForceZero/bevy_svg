use bevy::prelude::{Deref, DerefMut, Resource};
use crate::svg::Options;

#[derive(Resource, Deref, DerefMut, Default)]
pub struct FillTessellator(lyon_tessellation::FillTessellator);

#[derive(Resource, Deref, DerefMut, Default)]
pub struct StrokeTessellator(lyon_tessellation::StrokeTessellator);

#[derive(Resource, Debug, Default)]
pub struct SvgOptions {
    options: Option<Options>,
}