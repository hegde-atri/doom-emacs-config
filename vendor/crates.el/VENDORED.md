# Vendored crates.el

Source: https://github.com/shadr/crates.el
Commit: 4bbcb0db6a1aecba4a0c196eb2415959821fe65b
License: GPL-3.0-or-later, preserved in LICENSE

Local changes:
- URL-encode crate names before querying crates.io.
- Show overlays immediately after dependency version strings.
- Keep zero-width overlays live so virtual text remains visible.
- Parse pinned versions such as `=1.0.0`.
- Use theme-aware faces.
- Use ASCII overlay markers.
