{ callPackage
, pandoc
, nbconvert
, texliveToUse
}:


let
  common = callPackage ../../kernels/common.nix {};

in

common.writeShellScriptBinWithAttrs {
  name = "codedown-exporter-slidy";
  extension = "html";
  display_name = "Slidy (.html)";
  meta = nbconvert.meta;
  icon = null;
} "export" ''
  echo_and_run() { echo "$*" ; "$@" ; }
  echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"
  echo_and_run ${pandoc}/bin/pandoc -f markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart \
    -t slidy \
    -V slidy-url=https://www.w3.org/Talks/Tools/Slidy2 \
    --standalone \
    "--mathjax=https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_CHTML-full" \
    "$1" \
    "-o" "$2"
''
