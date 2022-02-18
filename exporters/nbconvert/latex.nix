{ callPackage
, writeTextFile
, pandoc
, nbconvert
}:

let
  common = callPackage ../../languages/common.nix {};

  attrs = {
    name = "codedown-latex";
    displayName = "CodeDown LaTeX exporter";
    meta = nbconvert.meta;
    icon = null;
  };

in

common.writeShellScriptBinWithAttrs attrs "lib/codedown/exporters/nbconvert_beamer" ''
  ${nbconvert}/bin/jupyter-nbconvert "$1" --to latex --stdout > "$2"
''
