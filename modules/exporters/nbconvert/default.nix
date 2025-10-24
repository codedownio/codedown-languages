{ callPackage
, python3
, symlinkJoin
, texliveScheme

, settings
, settingsSchema
}:

let
  nbconvert = python3.pkgs.nbconvert;

  common = callPackage ../../kernels/common.nix {};

  nbconvertExporter =
    common.writeShellScriptBinWithAttrs {
      meta = nbconvert.meta;
    } "export" ''
      echo_and_run() { echo "$*" ; "$@" ; }
      echo_and_run export PATH="''${PATH:+''${PATH}:}:${texliveScheme}/bin"

      filename=$(basename -- "$2")
      EXTENSION="''${filename##*.}"
      FILENAME="''${filename%.*}"

      echo "FILENAME: $FILENAME"
      echo "EXTENSION: $EXTENSION"

      echo_and_run ${nbconvert}/bin/jupyter-nbconvert "$1" --output "$FILENAME"
    '';

in

symlinkJoin {
  name = "nbconvert-exporters";
  paths = nbconvertExporter;

  passthru = {
    meta = {
      name = "nbconvert-exporters";
      description = "CodeDown exporters for PDF, HTML, LaTeX, slides, etc.";

      icon = ../../../codedown.png;
      iconMonochrome = ../../../codedown-monochrome.svg;

      # To separate these out in search results
      category = "Exporters";

      exporterInfo = {
        name = "codedown-nbconvert-exporter";
        display_name = "Nbconvert exporter";

        input_extension = "ipynb";
        output_extensions = [
          { ext = "pdf";      title = "PDF (.pdf)";              args = ["--to" ""]; }
          { ext = "asciidoc"; title = "AsciiDoc (.asciidoc)";    args = ["--to" ""]; }
          { ext = "html";     title = "HTML (.html)";            args = ["--to" ""]; }
          { ext = "html";     title = "Slides (.html)";          args = ["--to" ""]; }
          { ext = "md";       title = "Markdown (.md)";          args = ["--to" ""]; }
          { ext = "rst";      title = "reStructuredText (.rst)"; args = ["--to" ""]; }
          { ext = "tex";      title = "LaTeX (.tex)";            args = ["--to" ""]; }
        ];

        meta = nbconvert.meta;
        icon = null;
        args = [(nbconvertExporter + "/bin/export")];
        inputs = ["ipynb"];
      };
    };

    versions = {
      nbconvert = nbconvert.version;
    };

    inherit settingsSchema settings;
  };
}
