{ callPackage
, writeTextFile
, pandoc
, nbconvert
, runtimeShell
}:

let
  common = callPackage ../../languages/common.nix {};

  attrs = {
    name = "codedown-latex";
    displayName = "LaTeX (.tex)";
    meta = nbconvert.meta;
    icon = null;
  };

in

common.writeShellScriptBinWithAttrs attrs "export" ''
  export PATH="''${PATH:+''${PATH}:}${pandoc}/bin/pandoc"
  ${nbconvert}/bin/jupyter-nbconvert "$1" --to latex --stdout > "$2"
''
