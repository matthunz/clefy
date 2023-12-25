# Clefy

![Example image](https://raw.githubusercontent.com/matthunz/clefy/main/image.svg)

```rust
use clefy::{FontMetrics, Renderer};

fn main() -> std::io::Result<()> {
    let font_metrics = FontMetrics::load("assets/leland_metadata.json", "assets/glyphnames.json")?;
    let mut renderer = Renderer::from_path("assets/Leland.otf")?;

    font_metrics
        .glyph("noteheadBlack")
        .scale(1000.)
        .draw(&mut renderer, 0., 0.);

    font_metrics
        .glyph("noteheadHalf")
        .scale(1000.)
        .draw(&mut renderer, 100., 0.);

    svg::save("image.svg", renderer.document())?;
    Ok(())
}
```
