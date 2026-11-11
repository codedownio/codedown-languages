{ callPackage
, coreutils
, gnused

, typstToUse
, codedownPackagePath
}:

let
  common = callPackage ../../kernels/common.nix {};

in

# The render program behind the typst-export-server language server (see the RENDER_* variables in
# codedownio/lsp-export-server). The buffer goes in on stdin, so imports resolve from the document's
# directory (the working directory) rather than the work directory's copy; the epoch pins the PDF's
# timestamps. Image formats render the first page only, since a single output file can't hold more.
common.writeShellScriptBinWithAttrs {} "typst-render" ''
  set -euo pipefail

  export SOURCE_DATE_EPOCH="$RENDER_EPOCH"
  extra=()
  case "$RENDER_FORMAT" in
    png|svg) extra=(--pages 1) ;;
    html) extra=(--features html) ;;
  esac
  deps="$RENDER_WORK/deps.mk"

  cmd=(${typstToUse}/bin/typst compile --package-path ${codedownPackagePath} --format "$RENDER_FORMAT" "''${extra[@]}" --deps "$deps" --deps-format make - "$RENDER_OUTPUT")
  echo "''${cmd[*]} < $RENDER_ENTRY"
  "''${cmd[@]}" < "$RENDER_ENTRY"

  # The make-format line is "output: dep dep <stdin>"
  ${gnused}/bin/sed -e 's/^[^:]*: //' -e 's/ *<stdin>//' "$deps" | ${coreutils}/bin/tr ' ' '\n' > "$RENDER_DEPS"
''
