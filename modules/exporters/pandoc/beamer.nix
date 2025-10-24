{ callPackage
, pandoc
, texliveToUse
, writeTextFile
}:


let
  common = callPackage ../../kernels/common.nix {};

  customLatexHeader = writeTextFile {
    name = "latex_header.tex";
    text = ''
      \usepackage{graphicx}
      \usepackage{amssymb}
    '';
  };

  customLatexBody = writeTextFile {
    name = "latex_header.tex";
    text = ''
      \newcommand{\hole}{\square}
    '';
  };

in

common.writeShellScriptBinWithAttrs {
  name = "codedown-exporter-beamer";
  extension = "pdf";
  display_name = "Beamer slides (.pdf)";
  meta = pandoc.meta;
  icon = null;
} "export" ''
  echo_and_run() { echo "$*" ; "$@" ; }
  echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"
  echo_and_run ${pandoc}/bin/pandoc \
    -t beamer \
    --include-in-header ${customLatexHeader} \
    --include-before-body ${customLatexBody} \
    --table-of-contents \
    --standalone \
    "$1" \
    "-o" "$2"
''
