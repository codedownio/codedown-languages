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

      ghc-syntax-highlighter = let
        src = fetchFromGitHub {
          owner = "mrkkrp";
          repo = "ghc-syntax-highlighter";
          rev = "bbc049904524aae08e6431494f41fe2a288f6259";
          sha256 = "sha256-w7AxGsUfqGhh7wrSPppQ2+gPwjvb4mwExJdDOcasAZ4=";
        };
      in
        self.callCabal2nix "ghc-syntax-highlighter" src {};

      zeromq4-haskell = super.zeromq4-haskell.overrideAttrs (oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [libsodium];
      });
    };
  };

  ghc96 = haskell.packages.ghc96.override {
    overrides = self: super: {
      ghc-parser = let
        ghc-parser-source = runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out";
      in
        self.callCabal2nix "ghc-parser" ghc-parser-source {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

      ghc-lib-parser = self.ghc-lib-parser_9_6_3_20231014;
      ghc-lib-parser-ex = self.ghc-lib-parser-ex_9_6_0_2;

      ghc-syntax-highlighter = let
        src = fetchFromGitHub {
          owner = "mrkkrp";
          repo = "ghc-syntax-highlighter";
          rev = "71ff751eaa6034d4aef254d6bc5a8be4f6595344";
          sha256 = "wQmWSuvIJpg11zKl1qOSWpqxjp2DoJwa20vaS2KHypM=";
        };
      in
        self.callCabal2nix "ghc-syntax-highlighter" src {};

      zeromq4-haskell = super.zeromq4-haskell.overrideAttrs (oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [libsodium];
      });
    };
  };
}
