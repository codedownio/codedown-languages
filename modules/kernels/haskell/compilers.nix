{ lib
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
      overrides = self: _super: {
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
      overrides = self: _super: {
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
      overrides = self: _super: {
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

      cryptonite = super.cryptonite.overrideAttrs (_oldAttrs: {
        doCheck = false;
      });
    };
  };

  ghc914 = haskell.packages.ghc914.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

      cryptonite = super.cryptonite.overrideAttrs (_oldAttrs: {
        doCheck = false;
      });
    } // lib.genAttrs [
      # Each of these only trips an upper bound (base, containers, time, template-haskell,
      # tagged, hedgehog) under 9.14; the code itself compiles. Drop entries as nixpkgs
      # picks up releases with widened bounds.
      "algebraic-graphs"
      "apply-refact"
      "binary-instances"
      "Cabal-syntax_3_14_2_0"
      "clay"
      "constraints-extras"
      "dec"
      "ghc-lib-parser"
      "ghc-trace-events"
      "hie-compat"
      "lucid"
      "rebase"
      "singleton-bool"
      "string-interpolate"
      "tasty-hspec"
      "tomland"
    ] (name: haskell.lib.doJailbreak super.${name})
    // lib.genAttrs [
      # Only the test suites break under 9.14: generic-lens and relude fail doctests,
      # enummapset's tests import a module containers 0.8 now hides.
      "enummapset"
      "generic-lens"
      "relude"
    ] (name: haskell.lib.dontCheck super.${name});
  };
}
