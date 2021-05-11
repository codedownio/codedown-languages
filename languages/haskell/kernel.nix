{jupyter-kernel, haskell}:

jupyter-kernel.create {
  definitions = {
    haskell = {
      displayName = "Haskell";
      argv = [
        "${haskell.haskellPackages.ihaskell.components.exes.ihaskell}/bin/ihaskell"
        "kernel"
        "{connection_file}"
        "--stack"
        "+RTS" "-M3g" "-N2" "-RTS"
      ];
      language = "haskell";
      logo32 = null;
      logo64 = ./logo-64x64.svg;
      metadata = {
        codedown = {
          priority = 1;
        };
      };
    };
  };
};
