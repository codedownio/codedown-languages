{ callPackage
, gawk
, gnugrep
}:

let
  common = callPackage ../../kernels/common.nix {};

in

# The render program behind the latex-export-server language server (see the RENDER_* variables in
# codedownio/lsp-export-server): one engine pass for the quick phase, latexmk for the settle phase.
# The unsaved buffers come first on TEXINPUTS so they shadow their disk copies, the epoch pins the
# PDF's timestamps, and the recorder's INPUT lines become the dependency list. LATEX_RENDER_INPUT
# lets another program (pandoc-render) hand over a .tex it generated.
common.writeShellScriptBinWithAttrs {} "latex-render" ''
  set -euo pipefail

  tex="''${LATEX_RENDER_INPUT:-$RENDER_ENTRY}"
  engine="''${RENDER_OPT_engine:-pdflatex}"
  export TEXINPUTS="$RENDER_OVERLAY//:''${TEXINPUTS:-}"
  export LC_ALL=C SOURCE_DATE_EPOCH="$RENDER_EPOCH" FORCE_SOURCE_DATE=1

  common=(-interaction=nonstopmode -halt-on-error -file-line-error -recorder "-jobname=$RENDER_JOB")
  case "$RENDER_PHASE" in
    settle)
      case "$engine" in
        xelatex) flag=-pdfxe ;;
        lualatex) flag=-pdflua ;;
        *) flag=-pdf ;;
      esac
      cmd=(latexmk "$flag" "-outdir=$RENDER_OUT" "''${common[@]}" "$tex")
      ;;
    *)
      cmd=("$engine" "-output-directory=$RENDER_OUT" "''${common[@]}" "$tex")
      ;;
  esac

  echo "''${cmd[*]}"
  if ! "''${cmd[@]}"; then
    # The engine's own account of what went wrong: the ! lines and the file:line: errors
    log="$RENDER_OUT/$RENDER_JOB.log"
    [ -f "$log" ] && ${gnugrep}/bin/grep -E -B1 '^!|:error:|\.tex:[0-9]+:' "$log" >&2 || true
    exit 1
  fi

  ${gawk}/bin/awk '/^INPUT /{print $2}' "$RENDER_OUT/$RENDER_JOB.fls" > "$RENDER_DEPS"
''
