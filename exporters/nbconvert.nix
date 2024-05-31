{ callPackage
, symlinkJoin
, pandoc
, python3
, texlive
, size ? "small"
}:

let
  nbconvert = python3.pkgs.nbconvert.overrideAttrs (oldAttrs: {
    patches = oldAttrs.patches ++ [
      ./fix-asciidoc.patch
    ];
  });

  common = callPackage ../languages/common.nix {};

  texliveToUse = if size == "small" then texlive.combined.scheme-small else texlive.combined.scheme-full;

  makeNbconvertExporter = name: displayName: extension: to: common.writeShellScriptBinWithAttrs {
    inherit name extension;
    display_name = displayName;
    meta = nbconvert.meta;
    icon = null;
  } "export" ''
    echo_and_run() { echo "$*" ; "$@" ; }
    echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"
    echo_and_run ${nbconvert}/bin/jupyter-nbconvert "$1" --to ${to}
  '';

  exporters = [
    (makeNbconvertExporter "codedown-exporter-asciidoc" "AsciiDoc (.asciidoc)" "asciidoc" "asciidoc")
    (makeNbconvertExporter "codedown-exporter-latex" "LaTeX (.tex)" "tex" "latex")
    (makeNbconvertExporter "codedown-exporter-pdf" "PDF (.pdf)" "pdf" "pdf")
    (makeNbconvertExporter "codedown-exporter-html" "HTML (.html)" "html" "html")
    (makeNbconvertExporter "codedown-exporter-rst" "reStructuredText (.rst)" ".rst" "rst")
    (makeNbconvertExporter "codedown-exporter-slides" "Slides (.html)" ".html" "slides")
    (callPackage ./nbconvert/slidy.nix { inherit texliveToUse nbconvert; })
    (callPackage ./nbconvert/beamer.nix { inherit texliveToUse; })
    (makeNbconvertExporter "codedown-exporter-markdown" "Markdown (.md)" ".md" "markdown")
  ];

in

symlinkJoin {
  name = "nbconvert-exporters-" + size;
  paths = exporters;

  meta = {
    name = "nbconvert-exporters-" + size;
    description = "CodeDown exporters using a ${size} TeX Live distribution";

    icon = ../codedown-icon.png;

    # To separate these out in search results
    category = "Exporters";

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
