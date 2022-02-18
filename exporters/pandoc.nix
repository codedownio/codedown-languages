{ callPackage
, symlinkJoin
}:

let
  exporters = [
    (callPackage ./pandoc/slidy.nix {})
    (callPackage ./pandoc/beamer.nix {})
  ];

in

symlinkJoin {
  name = "codedown-pandoc-exporters";
  paths = exporters;

  meta = {
    exporterInfos = map (x: {
      name = x.name;
      displayName = x.displayName;
      meta = x.meta;
      icon = x.icon;
    }) exporters;
  };
}
