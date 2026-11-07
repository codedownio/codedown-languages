{ callPackage
, coreutils
, gnugrep
, jq
, lib
, runCommand
, symlinkJoin
, texliveToUse

, settings
, settingsSchema
}:

with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../../kernels/common.nix {};

  # codedown.sty, found through TEXINPUTS by both the export and texlab, so documents just
  # \usepackage{codedown}.
  codedownTexInputs = runCommand "codedown-latex-package" {} ''
    mkdir -p "$out/tex/latex/codedown"
    cp ${./codedown.sty} "$out/tex/latex/codedown/codedown.sty"
  '';
  texInputs = "${codedownTexInputs}//:";

  engineFlag = {
    pdflatex = "-pdf";
    xelatex = "-pdfxe";
    lualatex = "-pdflua";
  }.${settings.engine};

  # Writes "<output>.deps" in the typst_json shape ({"inputs": [...]}, paths relative to cwd), from
  # latexmk's .fls recorder file, so the runner re-renders when an \input file changes.
  script = common.writeShellScriptBinWithAttrs {} "latex-export" ''
    set -euo pipefail
    export PATH="${texliveToUse}/bin:${coreutils}/bin:${gnugrep}/bin''${PATH:+:''${PATH}}"
    export TEXINPUTS="${texInputs}''${TEXINPUTS:-}"

    input="$(realpath "$1")"
    output="$2"
    jobname="$(basename "$input" .tex)"
    build="$(dirname "$(realpath -m "$output")")/latex-build"
    mkdir -p "$build"

    echo latexmk ${engineFlag} -recorder -cd -outdir="$build" "$input"
    latexmk ${engineFlag} -recorder -interaction=nonstopmode -halt-on-error -file-line-error \
      -cd -outdir="$build" "$input"

    cp "$build/$jobname.pdf" "$output"

    docdir="$(dirname "$input")"
    grep '^INPUT ' "$build/$jobname.fls" | cut -c7- | sort -u | while IFS= read -r f; do
      case "$f" in
        /nix/store/*) continue ;;
        /*) ;;
        *) f="$docdir/$f" ;;
      esac
      case "$f" in "$build"/*) continue ;; esac
      if [ -f "$f" ]; then realpath --relative-to="$PWD" "$f"; fi
    done | sort -u | ${jq}/bin/jq -R . | ${jq}/bin/jq -s '{inputs: .}' > "$output.deps"
  '';

  languageServers = lib.optionals settings.lsp.texlab.enable
    [(callPackage ./language_server_texlab {
      inherit texInputs;
    })];

  icon = ../../../codedown.png;
  iconMonochrome = ../../../codedown-monochrome.svg;

  pdfExporter = {
    name = "codedown-exporter-latex";
    display_name = "PDF (.pdf)";
    group = "LaTeX";
    extension = "pdf";
    inherit icon;
    icon_monochrome = iconMonochrome;
    args = [(script + "/bin/latex-export")];
    input_extensions = ["tex"];
    deps = "typst_json";
  };

in

symlinkJoin {
  name = "codedown-exporter-latex";
  paths = [script] ++ languageServers;

  passthru = {
    meta = {
      name = "codedown-exporter-latex";
      description = "CodeDown exporter compiling LaTeX notebooks with latexmk.";

      # To separate these out in search results
      category = "Exporters";

      inherit icon iconMonochrome;

      exporterInfos = [pdfExporter];
    };

    versions = {
      texlive = texliveToUse.version or "";
    };

    inherit settingsSchema settings;

    modes = {
      inherit attrs extensions;
      code_mirror_mode = "stex";
    };

    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
