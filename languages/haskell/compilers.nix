{ fetchFromGitHub
, libsodium
, runCommand
, haskell

, ihaskell-source
}:

{
  # ghc810 = haskell.packages.ghc810.override {
  #   overrides = self: super: {
  #     ghc-parser = self.callCabal2nix "ghc-parser" (
  #       runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
  #     ) {};

  #     ghc-lib-parser = let
  #       src = fetchFromGitHub {
  #         owner = "digital-asset";
  #         repo = "ghc-lib";
  #         rev = "bbc049904524aae08e6431494f41fe2a288f6259";
  #         sha256 = "sha256-w7AxGsUfqGhh7wrSPppQ2+gPwjvb4mwExJdDOcasAZ4=";
  #       };
  #     in
  #       self.callCabal2nix "ghc-lib-parser" src {};

  #     ipython-kernel = self.callCabal2nix "ipython-kernel" (
  #       runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
  #     ) {};

  #     ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};
  #   };
  # };

  ghc90 = haskell.packages.ghc90.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};
    };
  };

  ghc92 = haskell.packages.ghc92.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};
    };
  };

  ghc94 = haskell.packages.ghc94.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

      zeromq4-haskell = super.zeromq4-haskell.overrideAttrs (oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [libsodium];
      });
    };
  };

  ghc96 = haskell.packages.ghc96.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

      warp_3_3_29 = null;
    };
  };

  ghc98 = haskell.packages.ghc98.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

      warp_3_3_29 = null;

      ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_11_0;
    };
  };
}
