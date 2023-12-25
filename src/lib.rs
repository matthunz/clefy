use rusttype::Font;
use std::{fs::File, io::Read};
use svg::Document;

pub mod font;
pub use font::FontMetrics;

pub struct Renderer {
    font: Font<'static>,
    document: Option<Document>,
}

impl Renderer {
    pub fn new() -> Self {
        let mut file = File::open("Leland.otf").unwrap();
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).unwrap();

        let font = Font::try_from_vec(buf).unwrap();
        Self {
            font,
            document: Some(Document::new()),
        }
    }

    pub fn document(&self) -> &Document {
        self.document.as_ref().unwrap()
    }
}
