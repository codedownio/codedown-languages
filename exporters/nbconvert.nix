{ callPackage
, symlinkJoin
, pandoc
, python3
, texlive
, size ? "small"
}:

let
  nbconvert = python3.pkgs.nbconvert;

  common = callPackage ../languages/common.nix {};

  texliveToUse = if size == "small" then texlive.combined.scheme-small else texlive.combined.scheme-full;

  makeNbconvertExporter = name: displayName: extension: to: common.writeShellScriptBinWithAttrs {
    inherit name extension;
    display_name = displayName;
    meta = nbconvert.meta;
    icon = null;
  } "export" ''
    echo "export PATH=\"''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin\""
    export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"

    echo "${nbconvert}/bin/jupyter-nbconvert $1 --to ${to}"
    ${nbconvert}/bin/jupyter-nbconvert "$1" --to ${to}
  '';

  slidyExporter = common.writeShellScriptBinWithAttrs {
    name = "codedown-exporter-slidy";
    extension = "html";
    display_name = "Slidy (.html)";
    meta = nbconvert.meta;
    icon = null;
  } "export" ''
    echo "export PATH=\"''${PATH:+''${PATH}:}${texliveToUse}/bin\""
    export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"

    ${pandoc}/bin/pandoc -f markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart \
      -t slidy \
      -V slidy-url=https://www.w3.org/Talks/Tools/Slidy2 \
      -s \
      --mathjax=https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_CHTML-full \
      "$1" \
      "-o" "$2"
  '';

  exporters = [
    (makeNbconvertExporter "codedown-exporter-asciidoc" "AsciiDoc (.asciidoc)" "asciidoc" "asciidoc")
    (makeNbconvertExporter "codedown-exporter-latex" "LaTeX (.tex)" "tex" "latex")
    (makeNbconvertExporter "codedown-exporter-pdf" "PDF (.pdf)" "pdf" "pdf")
    (makeNbconvertExporter "codedown-exporter-html" "HTML (.html)" "html" "html")
    (makeNbconvertExporter "codedown-exporter-rst" "reStructuredText (.rst)" ".rst" "rst")
    (makeNbconvertExporter "codedown-exporter-slides" "Slides (.html)" ".html" "slides")
    slidyExporter
    (callPackage ./nbconvert/beamer.nix { inherit texliveToUse; })
    (makeNbconvertExporter "codedown-exporter-markdown" "Markdown (.md)" ".md" "markdown")
  ];

in

symlinkJoin {
  name = "nbconvert-exporters-" + size;
  paths = exporters;

  meta = {
    exporterInfos = map (x: {
      name = x.name;
      display_name = x.display_name;
      extension = x.extension;
      meta = x.meta;
      icon = x.icon;
      args = [(x + "/bin/export")];
    }) exporters;
  };
}
