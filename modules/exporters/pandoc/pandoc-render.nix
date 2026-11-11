{ callPackage
, pandoc

, latexRender
}:

let
  common = callPackage ../../kernels/common.nix {};

in

# The render program behind the pandoc-export-server language server (see the RENDER_* variables in
# codedownio/lsp-export-server). Options: to (the writer; pdf means latex), from (the reader for
# Markdown input), pandoc_args (extra arguments, space-separated), and engine for the LaTeX-based
# writers, which hand the generated .tex to latex-render for both phases. The other writers have
# nothing to settle.
common.writeShellScriptBinWithAttrs {} "pandoc-render" ''
  set -euo pipefail

  to="''${RENDER_OPT_to:-html}"
  writer="$to"
  [ "$to" = pdf ] && writer=latex
  case "$writer" in
    latex|beamer) via_latex=1; out="$RENDER_SRC/$RENDER_JOB.tex" ;;
    *) via_latex=0; out="$RENDER_OUTPUT" ;;
  esac
  if [ "$RENDER_PHASE" = settle ] && [ "$via_latex" = 0 ] && [ -f "$RENDER_OUTPUT" ]; then
    exit 0
  fi

  reader=()
  case "$RENDER_ENTRY" in
    *.md) reader=(-f "''${RENDER_OPT_from:-markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart}") ;;
  esac
  extra=()
  if [ -n "''${RENDER_OPT_pandoc_args:-}" ]; then
    read -r -a extra <<< "$RENDER_OPT_pandoc_args"
  fi

  export LC_ALL=C
  cmd=(${pandoc}/bin/pandoc "''${reader[@]}" -t "$writer" --standalone "''${extra[@]}" "$RENDER_ENTRY" -o "$out")
  echo "''${cmd[*]}"
  "''${cmd[@]}"

  if [ "$via_latex" = 1 ]; then
    LATEX_RENDER_INPUT="$out" exec ${latexRender}/bin/latex-render
  fi
''
