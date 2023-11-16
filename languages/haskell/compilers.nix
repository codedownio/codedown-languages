{ fetchFromGitHub
, libsodium
, runCommand
, haskell

, ihaskell-source
}:

{
  ghc810 = haskell.packages.ghc810.override {
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

      here = super.here.overrideAttrs (oldAttrs: {
        src = fetchFromGitHub {
          owner = "tmhedberg";
          repo = "here";
          rev = "2530d70b44b23dc6f3dfbc762a8199e70b952e1c";
          sha256 = "q6oneTExLJw6P7iwwkHJCAN/MS69B0uw4r97fA49Jcw=";
        };
        doCheck = false;
        buildInputs = oldAttrs.buildInputs ++ [super.hspec];
      });

      hlint = super.shelly.overrideAttrs (oldAttrs: {
        src = fetchFromGitHub {
          owner = "ndmitchell";
          repo = "hlint";
          rev = "ed1259a7da88420e8d05d6241d6bdd4493a9997f";
          sha256 = "kLqO2Hbm2ekNzNgT54oZfF8EleqvvxoFlWjJTiteBzI=";
        };
      });

      ihaskell = super.ihaskell.overrideAttrs (oldAttrs: {
        src = ihaskell-source;
      });

      shelly = super.shelly.overrideAttrs (oldAttrs: {
        src = fetchFromGitHub {
          owner = "gregwebs";
          repo = "Shelly.hs";
          rev = "db62da933cba5da2a6aed34f049685fc72cb8440";
          sha256 = "VOYIH9hzAL98x1nmFHWsKUSapq/UEj1ZhjSqk0JECPg=";
        };
      });

      zeromq4-haskell = super.zeromq4-haskell.overrideAttrs (oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [libsodium];
      });
    };
  };
}
