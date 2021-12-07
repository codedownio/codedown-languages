{ lib
, jupyter-kernel
, snapshot
, attrs
, extensions
, displayName
, metaOnly ? false
}:

jupyter-kernel.create {
  definitions = {
    haskell = {
      inherit displayName;
      argv = [
        "${snapshot.ihaskell.components.exes.ihaskell}/bin/ihaskell"
        "kernel"
        "{connection_file}"
        "--stack"
        "+RTS" "-M3g" "-N2" "-RTS"
      ];
      language = lib.head attrs;
      logo32 = null;
      logo64 = ./logo-64x64.svg;
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
    };
  };
}
