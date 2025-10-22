{ callPackage
, pandoc
, python3
, symlinkJoin
, texliveScheme

, settings
, settingsSchema
}:

let
  nbconvert = python3.pkgs.nbconvert;

  common = callPackage ../../kernels/common.nix {};

  makeNbconvertExporter = name: displayName: extension: to:
    common.writeShellScriptBinWithAttrs {
      inherit name extension;
      display_name = displayName;
      meta = nbconvert.meta;
      icon = null;
    } "export" ''
      echo_and_run() { echo "$*" ; "$@" ; }
      echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveScheme}/bin"

      filename=$(basename -- "$2")
      EXTENSION="''${filename##*.}"
      FILENAME="''${filename%.*}"

      echo "FILENAME: $FILENAME"
      echo "EXTENSION: $EXTENSION"

      echo_and_run ${nbconvert}/bin/jupyter-nbconvert "$1" --output "$FILENAME" --to ${to}
    '';

  exporters = [
    (makeNbconvertExporter "codedown-exporter-asciidoc" "AsciiDoc (.asciidoc)" "asciidoc" "asciidoc")
    (makeNbconvertExporter "codedown-exporter-latex" "LaTeX (.tex)" "tex" "latex")
    (makeNbconvertExporter "codedown-exporter-pdf" "PDF (.pdf)" "pdf" "pdf")
    (makeNbconvertExporter "codedown-exporter-html" "HTML (.html)" "html" "html")
    (makeNbconvertExporter "codedown-exporter-rst" "reStructuredText (.rst)" ".rst" "rst")
    (makeNbconvertExporter "codedown-exporter-slides" "Slides (.html)" ".html" "slides")
    (callPackage ./slidy.nix {
      inherit nbconvert;
      texliveToUse = texliveScheme;
    })
    (callPackage ./beamer.nix {
      texliveToUse = texliveScheme;
    })
    (makeNbconvertExporter "codedown-exporter-markdown" "Markdown (.md)" ".md" "markdown")
  ];

in

symlinkJoin {
  name = "nbconvert-exporters";
  paths = exporters;

  passthru = {
    meta = {
      name = "nbconvert-exporters";
      description = "CodeDown exporters for PDF, HTML, LaTeX, slides, etc.";

      icon = ../../../codedown.png;
      iconMonochrome = ../../../codedown-monochrome.svg;

      # To separate these out in search results
      category = "Exporters";

      exporterInfos = map (x: {
        name = x.name;
        display_name = x.display_name;
        extension = x.extension;
        meta = x.meta;
        icon = x.icon;
        args = [(x + "/bin/export")];
        inputs = ["ipynb"];
      }) exporters;
    };

    versions = {
      nbconvert = nbconvert.version;
    };

    inherit settingsSchema settings;
  };
}
