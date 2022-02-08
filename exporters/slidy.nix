{ writeShellScriptBin
, pandoc }:


{
  name = "codedown-slidy-exporter";
  displayName = "CodeDown Slidy Exporter";


  contents = writeShellScriptBin "export" ''
    ${pandoc}/bin/pandoc -f markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart \
      -t slidy \
      -V slidy-url=https://www.w3.org/Talks/Tools/Slidy2 \
      -s \
      --mathjax=https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_CHTML-full \
      "$1" \
      "-o" "$2"
  '';
}
