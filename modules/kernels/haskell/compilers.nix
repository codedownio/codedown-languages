{ fetchFromGitHub
, lib
, libsodium
, runCommand
, haskell

, ihaskell-source
}:

{
  ghc96 = let
    "ghc-syntax-highlighter_0_0_11_0" = haskell.packages.ghc96.callPackage
      ({ mkDerivation, base, ghc-lib-parser, hspec, hspec-discover, text
       }:
         mkDerivation {
           pname = "ghc-syntax-highlighter";
           version = "0.0.11.0";
           sha256 = "sha256-umeX9DNHPNQ3D66C3WPKZyo4NKOl7XyHz61V1QyHW3g=";
           enableSeparateDataOutput = true;
           libraryHaskellDepends = [ base ghc-lib-parser text ];
           testHaskellDepends = [ base hspec text ];
           testToolDepends = [ hspec-discover ];
           description = "Syntax highlighter for Haskell using the lexer of GHC";
           license = lib.licenses.bsd3;
           hydraPlatforms = lib.platforms.none;
         }) {};
  in
    haskell.packages.ghc96.override {
      overrides = self: super: {
        ghc-parser = self.callCabal2nix "ghc-parser" (
          runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
        ) {};

        ipython-kernel = self.callCabal2nix "ipython-kernel" (
          runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
        ) {};

        ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

        ghc-syntax-highlighter = ghc-syntax-highlighter_0_0_11_0;
      };
    };

  ghc98 = let
    "ghc-syntax-highlighter_0_0_11_0" = haskell.packages.ghc98.callPackage
      ({ mkDerivation, base, ghc-lib-parser, hspec, hspec-discover, text
       }:
         mkDerivation {
           pname = "ghc-syntax-highlighter";
           version = "0.0.11.0";
           sha256 = "sha256-umeX9DNHPNQ3D66C3WPKZyo4NKOl7XyHz61V1QyHW3g=";
           enableSeparateDataOutput = true;
           libraryHaskellDepends = [ base ghc-lib-parser text ];
           testHaskellDepends = [ base hspec text ];
           testToolDepends = [ hspec-discover ];
           description = "Syntax highlighter for Haskell using the lexer of GHC";
           license = lib.licenses.bsd3;
           hydraPlatforms = lib.platforms.none;
         }) {};
  in
    haskell.packages.ghc98.override {
      overrides = self: super: {
        ghc-parser = self.callCabal2nix "ghc-parser" (
          runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
        ) {};

        ipython-kernel = self.callCabal2nix "ipython-kernel" (
          runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
        ) {};

        ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

        ghc-syntax-highlighter = ghc-syntax-highlighter_0_0_11_0;
      };
    };

  ghc910 = haskell.packages.ghc910.override {
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

  ghc912 = haskell.packages.ghc912.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

      cryptonite = super.cryptonite.overrideAttrs (oldAttrs: {
        doCheck = false;
      });
    };
  };
}
