{ callPackage
, pandoc
, texliveToUse
, writeTextFile
}:


let
  common = callPackage ../../languages/common.nix {};

  customLatexHeader = writeTextFile {
    name = "latex_header.tex";
    text = ''
      \\usepackage{graphicx}
      \\usepackage{amssymb}
    '';
  };

  customLatexBody = writeTextFile {
    name = "latex_header.tex";
    text = ''
      \\newcommand{\\hole}{\\square}
    '';
  };

in

common.writeShellScriptBinWithAttrs {
  name = "codedown-exporter-beamer";
  extension = "html";
  display_name = "Beamer (.html)";
  meta = pandoc.meta;
  icon = null;
} "export" ''
  echo_and_run() { echo "$*" ; "$@" ; }
  echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"
  echo_and_run ${pandoc}/bin/pandoc -f markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart \
    -t beamer \
    --include-in-header ${customLatexHeader} \
    --include-in-before-body ${customLatexBody} \
    --table-of-contents \
    -s \
    "$1" \
    "-o" "$2"
''
