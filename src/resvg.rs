/// Instead of rewriting the wheel and because these aren't publicly exported from resvg, we defined them here

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// https://github.com/RazrFalcon/resvg/blob/1a6922d5bfcee9e69e04dc47cb0b586f1ca64a1c/crates/resvg/src/render.rs#L7-L9
#[derive(Debug, Copy, Clone)]
pub struct Context {
    pub max_bbox: tiny_skia::IntRect,
}

pub mod tiny_skia {
    pub use usvg::tiny_skia_path::*;
}

// https://github.com/RazrFalcon/resvg/blob/1a6922d5bfcee9e69e04dc47cb0b586f1ca64a1c/crates/resvg/src/geom.rs
pub mod geom {
    use super::tiny_skia::IntRect;
    /// Fits the current rect into the specified bounds.
    pub fn fit_to_rect(
        r: IntRect,
        bounds: IntRect,
    ) -> Option<IntRect> {
        let mut left = r.left();
        if left < bounds.left() {
            left = bounds.left();
        }

        let mut top = r.top();
        if top < bounds.top() {
            top = bounds.top();
        }

        let mut right = r.right();
        if right > bounds.right() {
            right = bounds.right();
        }

        let mut bottom = r.bottom();
        if bottom > bounds.bottom() {
            bottom = bounds.bottom();
        }

        IntRect::from_ltrb(left, top, right, bottom)
    }
}
