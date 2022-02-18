{ callPackage
, writeTextFile
, pandoc
}:

let
  common = callPackage ../../languages/common.nix {};

  attrs = {
    name = "codedown-beamer";
    displayName = "Beamer (.htm)";
    meta = pandoc.meta;
    icon = null;
  };

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

common.writeShellScriptBinWithAttrs attrs "export" ''
  ${pandoc}/bin/pandoc -f markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart \
    -t beamer \
    --include-in-header ${customLatexHeader}
    --include-in-before-body ${customLatexBody}
    --table-of-contents
    -s \
    "$1" \
    "-o" "$2"
''
