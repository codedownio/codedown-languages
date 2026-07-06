// CodeDown Typst prelude.
//
// Shipped as an @local package so documents can `#import "@local/codedown:0.1.0": *` instead of
// defining these helpers inline. CodeDown makes it resolvable by passing `--package-path` to both
// `typst compile` (export) and tinymist (live preview).

// A notebook code cell. Renders the cell's output (an image, raw block, etc.); the source code
// itself is not shown in the compiled document. (Matches the historical inline
// `#let codedown(code, output: none) = output`.)
#let codedown(code, output: none) = output

// An annotation anchor. Renders the wrapped span unchanged (transparent), so the anchor travels
// with the text through edits. CodeDown recovers the (id, span) by reading the source, not by
// querying the compiled document. The `..args` sink absorbs the annotation's serialized metadata
// (e.g. `comments: (...)`), which is data for CodeDown, not for rendering.
#let codedown_annotation(id, body, ..args) = body
