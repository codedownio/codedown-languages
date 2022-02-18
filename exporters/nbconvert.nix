{ callPackage
, symlinkJoin
, python3
}:

let
  nbconvert = python3.pkgs.nbconvert;

  exporters = [
    (callPackage ./nbconvert/latex.nix { inherit nbconvert; })
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
      args = [x];
    }) exporters;
  };
}
