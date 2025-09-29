{ fetchFromGitHub
, lib
, libsodium
, runCommand
, haskell

, ihaskell-source
}:

{
  ghc910 = let
    "ghc-syntax-highlighter_0_0_12_0" = haskell.packages.ghc910.callPackage
      ({ mkDerivation, base, ghc-lib-parser_9_10_2_20250515, hspec, hspec-discover, text
       }:
         mkDerivation {
           pname = "ghc-syntax-highlighter";
           version = "0.0.12.0";
           sha256 = "sha256-qZ1FkqZIeICogR5QTsILNWaZrnCvt6LLJQq66QnFdGA=";
           enableSeparateDataOutput = true;
           libraryHaskellDepends = [ base ghc-lib-parser_9_10_2_20250515 text ];
           testHaskellDepends = [ base hspec text ];
           testToolDepends = [ hspec-discover ];
           description = "Syntax highlighter for Haskell using the lexer of GHC";
           license = lib.licenses.bsd3;
           hydraPlatforms = lib.platforms.none;
         }) {};
  in
    haskell.packages.ghc910.override {
      overrides = self: super: {
        ghc-parser = self.callCabal2nix "ghc-parser" (
          runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
        ) {};

        ipython-kernel = self.callCabal2nix "ipython-kernel" (
          runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
        ) {};

        ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

        ghc-lib-parser = self.ghc-lib-parser_9_10_2_20250515;

        # We need a specific un-released version here; see
        # https://github.com/ndmitchell/hlint/issues/1613#issuecomment-3231912769
        hlint = let
          src = fetchFromGitHub {
            owner = "ndmitchell";
            repo = "hlint";
            rev = "7dfba720eaf6fa9bd0b23ae269334559aa722847";
            sha256 = "sha256-niGBdSrkatr+TZCcLYXo4MDg5FyXTYiKQ5K+ZIWSWBs=";
          };
        in
          self.callCabal2nix "hlint" src {};

        ghc-syntax-highlighter = ghc-syntax-highlighter_0_0_12_0;
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
