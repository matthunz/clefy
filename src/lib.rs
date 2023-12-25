use rusttype::Font;
use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};
use svg::Document;

pub mod font;
pub use font::FontMetrics;

pub struct Renderer {
    font: Font<'static>,
    document: Option<Document>,
}

impl Renderer {
    pub fn from_path(path: impl AsRef<Path>) -> io::Result<Self> {
        let mut file = File::open(path)?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf)?;

        let font = Font::try_from_vec(buf).ok_or(io::Error::from(io::ErrorKind::InvalidData))?;
        Ok(Self {
            font,
            document: Some(Document::new()),
        })
    }

    pub fn document(&self) -> &Document {
        self.document.as_ref().unwrap()
    }
}
