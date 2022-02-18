{ callPackage
, symlinkJoin
, pandoc
, python3
}:

let
  nbconvert = python3.pkgs.nbconvert;

  common = callPackage ../languages/common.nix {};

  makeNbconvertExporter = name: displayName: extension: to: common.writeShellScriptBinWithAttrs {
    inherit name extension;
    display_name = displayName;
    meta = nbconvert.meta;
    icon = null;
  } "export" ''
    export PATH="''${PATH:+''${PATH}:}${pandoc}/bin"
    ${nbconvert}/bin/jupyter-nbconvert "$1" --to ${to} --stdout > "$2"
  '';

  exporters = [
    (makeNbconvertExporter "codedown-exporter-latex" "LaTeX (.tex)" "tex" "latex")
    (makeNbconvertExporter "codedown-exporter-pdf" "PDF (.pdf)" "pdf" "pdf")
    (makeNbconvertExporter "codedown-exporter-html" "HTML (.html)" "html" "html")
    (makeNbconvertExporter "codedown-exporter-slides" "Slides (.html)" ".html" "slides")
  ];

in

symlinkJoin {
  name = "codedown-nbconvert-exporters";
  paths = exporters;

  meta = {
    exporterInfos = map (x: {
      name = x.name;
      display_name = x.displayName;
      meta = x.meta;
      icon = x.icon;
      args = [(x + "/bin/export")];
    }) exporters;
  };
}
