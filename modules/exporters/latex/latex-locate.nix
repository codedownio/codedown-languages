{ callPackage
, coreutils
, gawk
, gnugrep
, gzip
}:

let
  common = callPackage ../../kernels/common.nix {};

in

# The locate program for latex-export-server (RENDER_QUERY in codedownio/lsp-export-server)
common.writeShellScriptBinWithAttrs {} "latex-locate" ''
  set -euo pipefail

  case "$RENDER_QUERY" in
    forward)
      # synctex view falls back to nearby lines, so answer only for a line that produced output
      synctex=$(${gzip}/bin/gzip -dc "$RENDER_OUT/$RENDER_JOB.synctex.gz")
      src=$(${coreutils}/bin/realpath -m -- "$RENDER_SOURCE")
      tags=$(${gnugrep}/bin/grep '^Input:' <<< "$synctex" | while IFS=: read -r _ tag path; do
        if [ "$(${coreutils}/bin/realpath -m -- "$path")" = "$src" ]; then printf '%s|' "$tag"; fi
      done)
      if [ -z "$tags" ] || ! ${gnugrep}/bin/grep -qE "^[][(xkg\$hv](''${tags%|}),$RENDER_LINE[,:]" <<< "$synctex"; then
        : > "$RENDER_RESULT"
        exit 0
      fi

      # Every distinct output line synctex matches, on its first page
      synctex view -i "$RENDER_LINE:$RENDER_COLUMN:$RENDER_SOURCE" -o "$RENDER_OUTPUT" \
        | ${gawk}/bin/awk -F: '
            /^Page:/ { page = $2 }
            /^h:/ { h = $2 } /^v:/ { v = $2 } /^W:/ { w = $2 }
            /^H:/ {
              if (first == "") first = page
              box = sprintf("%d %.3f %.3f %.3f %.3f", page, h, v - $2, w, $2)
              if (page == first && w > 0 && $2 > 0 && !(box in seen)) { seen[box] = 1; print box }
            }' \
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
