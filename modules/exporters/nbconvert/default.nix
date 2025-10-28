{ callPackage
, coreutils
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
      icon = ./jupyter.svg;
      iconMonochrome = ./jupyter.svg;
    } "export" ''
      echo_and_run() { echo "$*" ; "$@" ; }
      echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveScheme}/bin"

      filename=$(${coreutils}/bin/basename -- "$2")
      EXTENSION="''${filename##*.}"
      FILENAME="''${filename%.*}"

      echo "FILENAME: $FILENAME"
      echo "EXTENSION: $EXTENSION"

      echo_and_run ${nbconvert}/bin/jupyter-nbconvert "$1" --output "$FILENAME" --to ${to}
    '';

  exporters = [
    (makeNbconvertExporter "codedown-exporter-nbconvert-asciidoc" "AsciiDoc (.asciidoc)" "asciidoc" "asciidoc")
    (makeNbconvertExporter "codedown-exporter-nbconvert-latex" "LaTeX (.tex)" "tex" "latex")
    (makeNbconvertExporter "codedown-exporter-nbconvert-pdf" "PDF (.pdf)" "pdf" "pdf")
    (makeNbconvertExporter "codedown-exporter-nbconvert-html" "HTML (.html)" "html" "html")
    (makeNbconvertExporter "codedown-exporter-nbconvert-rst" "reStructuredText (.rst)" ".rst" "rst")
    (makeNbconvertExporter "codedown-exporter-nbconvert-slides" "Slides (.html)" ".html" "slides")
    (makeNbconvertExporter "codedown-exporter-nbconvert-markdown" "Markdown (.md)" ".md" "markdown")
  ];

in

symlinkJoin {
  name = "nbconvert-exporters";
  paths = exporters;

  passthru = {
    meta = {
      name = "nbconvert-exporters";
      description = "CodeDown exporters for PDF, HTML, LaTeX, slides, etc.";

      # To separate these out in search results
      category = "Exporters";

      exporterInfos = map (x: {
        name = x.name;
        display_name = x.display_name;
        extension = x.extension;
        meta = x.meta // {
          inherit (x) icon iconMonochrome;
        };
        icon = x.icon;
        icon_monochrome = x.iconMonochrome;
        args = [(x + "/bin/export")];
        input_extensions = ["ipynb"];
      }) exporters;
    };

    versions = {
      nbconvert = nbconvert.version;
    };

    inherit settingsSchema settings;
  };
}
