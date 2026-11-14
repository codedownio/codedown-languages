{ callPackage
, gawk
}:

let
  common = callPackage ../../kernels/common.nix {};

in

# The locate program behind the latex-export-server language server (see RENDER_QUERY in
# codedownio/lsp-export-server), answered from the .synctex.gz the render left beside the PDF.
# Forward writes the first box the line set as "page left top width height"; inverse writes the
# source as "line column file". SyncTeX reports PDF points from the page's top-left corner, with
# (h, v) the box's left edge and baseline.
common.writeShellScriptBinWithAttrs {} "latex-locate" ''
  set -euo pipefail

  case "$RENDER_QUERY" in
    forward)
      synctex view -i "$RENDER_LINE:$RENDER_COLUMN:$RENDER_SOURCE" -o "$RENDER_OUTPUT" \
        | ${gawk}/bin/awk -F: '
            /^Page:/ { if (page != "") exit; page = $2 }
            /^h:/ { h = $2 } /^v:/ { v = $2 } /^W:/ { w = $2 } /^H:/ { ht = $2 }
            END { if (page != "") print page, h, v - ht, w, ht }' \
        > "$RENDER_RESULT"
      ;;
    inverse)
      synctex edit -o "$RENDER_PAGE:$RENDER_X:$RENDER_Y:$RENDER_OUTPUT" \
        | ${gawk}/bin/awk '
            /^Input:/ { if (input != "") exit; input = substr($0, 7) }
            /^Line:/ { line = substr($0, 6) } /^Column:/ { column = substr($0, 8) }
            END { if (input != "") print line, column, input }' \
        > "$RENDER_RESULT"
      ;;
  esac
''
