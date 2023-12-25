use serde::Deserialize;
use std::{collections::HashMap, fs::File, ops::Deref, path::Path};
use text_svg::Glpyh;

use crate::Renderer;

#[derive(Clone, Copy, Debug, Deserialize)]
pub struct Bounds {
    #[serde(rename = "bBoxNE")]
    pub ne: [f64; 2],
    #[serde(rename = "bBoxSW")]
    pub sw: [f64; 2],
}

impl Bounds {
    pub fn scale(self, size: f64) -> Self {
        Self {
            ne: [self.ne[0] * size, self.ne[1] * size],
            sw: [self.sw[0] * size, self.sw[1] * size],
        }
    }

    pub fn width(self) -> f64 {
        self.ne[0] - self.sw[0]
    }

    pub fn height(self) -> f64 {
        self.sw[1] - self.ne[1]
    }
}

// TODO Fx
#[derive(Debug, Deserialize)]
pub struct FontData {
    #[serde(rename = "glyphBBoxes")]
    pub glyph_bounding_boxes: HashMap<String, Bounds>,

    #[serde(rename = "glyphsWithAnchors")]
    pub glyphs_with_anchors: HashMap<String, HashMap<String, [f64; 2]>>,
}

#[derive(Debug, Deserialize)]
pub struct GlyphData {
    pub codepoint: String,
    pub description: String,
}

#[derive(Debug)]
pub struct FontMetrics {
    pub metrics: FontData,
    pub glyph_names: HashMap<String, GlyphData>,
}

impl FontMetrics {
    pub fn load(metrics_path: impl AsRef<Path>, glyphs_path: impl AsRef<Path>) -> Self {
        let metrics_file = File::open(metrics_path).unwrap();
        let metrics = serde_json::from_reader(metrics_file).unwrap();

        let glyphs_file = File::open(glyphs_path).unwrap();
        let glyph_names = serde_json::from_reader(glyphs_file).unwrap();

        Self {
            metrics,
            glyph_names,
        }
    }

    pub fn glyph<'a>(&'a self, name: &'a str) -> Glyph<'a> {
        Glyph { name, font: self }
    }
}

pub trait GlyphMetrics {
    fn stem_down(&self) -> Option<[f64; 2]>;

    fn stem_up(&self) -> Option<[f64; 2]>;
}

#[derive(Clone, Copy, Debug)]
pub struct Glyph<'a> {
    pub name: &'a str,
    pub font: &'a FontMetrics,
}

impl<'a> Glyph<'a> {
    pub fn anchors(&self) -> &HashMap<String, [f64; 2]> {
        &self.font.metrics.glyphs_with_anchors[self.name]
    }

    pub fn bounds(&self) -> Bounds {
        self.font.metrics.glyph_bounding_boxes[self.name]
    }

    pub fn as_char(&self) -> char {
        let codepoint = &self.font.glyph_names[self.name].codepoint;
        if &codepoint[0..2] != "U+" {
            todo!()
        }

        let code_point_str = &codepoint[2..];
        if let Ok(code_point) = u32::from_str_radix(code_point_str, 16) {
            std::char::from_u32(code_point).unwrap()
        } else {
            todo!()
        }
    }

    pub fn scale(self, size: f64) -> ScaledGlyph<'a> {
        ScaledGlyph { glyph: self, size }
    }
}

impl GlyphMetrics for Glyph<'_> {
    fn stem_down(&self) -> Option<[f64; 2]> {
        self.anchors().get("stemDownNW").copied()
    }

    fn stem_up(&self) -> Option<[f64; 2]> {
        self.anchors().get("stemUpSE").copied()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ScaledGlyph<'a> {
    pub glyph: Glyph<'a>,
    pub size: f64,
}

impl ScaledGlyph<'_> {
    pub fn draw(&mut self, renderer: &mut Renderer, x: f64, y: f64) {
        let glyph = Glpyh::new(&renderer.font, self.as_char(), self.size as _);
        let document = renderer.document.take().unwrap();
        renderer.document = Some(document.add(glyph.path(x as _, y as _)));
    }
}

impl<'a> Deref for ScaledGlyph<'a> {
    type Target = Glyph<'a>;

    fn deref(&self) -> &Self::Target {
        &self.glyph
    }
}

impl GlyphMetrics for ScaledGlyph<'_> {
    fn stem_down(&self) -> Option<[f64; 2]> {
        self.glyph.stem_down().map(|pos| scale(pos, self.size))
    }

    fn stem_up(&self) -> Option<[f64; 2]> {
        self.glyph.stem_up().map(|pos| scale(pos, self.size))
    }
}

fn scale(array: [f64; 2], size: f64) -> [f64; 2] {
    [array[0] * size, array[1] * size]
}
