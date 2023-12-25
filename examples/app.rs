use clefy::{FontMetrics, Renderer};

fn main() {
    let font_metrics = FontMetrics::load("leland_metadata.json", "glyphnames.json");
    let mut renderer = Renderer::new();

    font_metrics
        .glyph("noteheadBlack")
        .scale(1000.)
        .draw(&mut renderer, 0., 0.);

    font_metrics
        .glyph("noteheadHalf")
        .scale(1000.)
        .draw(&mut renderer, 100., 0.);

    svg::save("image.svg", renderer.document()).unwrap();
}
