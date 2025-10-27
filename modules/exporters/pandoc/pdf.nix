{ callPackage
, pandoc
, texliveToUse
}:


let
  common = callPackage ../../kernels/common.nix {};

in

common.writeShellScriptBinWithAttrs {
  name = "codedown-exporter-pdf";
  extension = "html";
  display_name = "Pandoc PDF (.pdf)";
  meta = pandoc.meta;
  icon = null;
} "export" ''
  echo_and_run() { echo "$*" ; "$@" ; }
  echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"
  echo_and_run ${pandoc}/bin/pandoc -f markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart \
    -t pdf \
    "$1" \
    "-o" "$2"
''
