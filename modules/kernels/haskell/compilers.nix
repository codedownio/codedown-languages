{ fetchFromGitHub
, lib
, libsodium
, runCommand
, haskell

, ihaskell-source
}:


let
  # "ghc-syntax-highlighter_0_0_9_0" = haskell.packages.ghc94.callPackage
  #   ({ mkDerivation, base, ghc-lib-parser, hspec, hspec-discover, text
  #    }:
  #      mkDerivation {
  #        pname = "ghc-syntax-highlighter";
  #        version = "0.0.9.0";
  #        sha256 = "sha256-Cjl2i2GWNPwWWYnKJq5xbFOULdMtQzsRdDem5wAAVjU=";
  #        enableSeparateDataOutput = true;
  #        libraryHaskellDepends = [ base ghc-lib-parser-948 text ];
  #        testHaskellDepends = [ base hspec text ];
  #        testToolDepends = [ hspec-discover ];
  #        description = "Syntax highlighter for Haskell using the lexer of GHC";
  #        license = lib.licenses.bsd3;
  #        hydraPlatforms = lib.platforms.none;
  #      }) {};


  "ghc-syntax-highlighter_0_0_12_0" = haskell.packages.ghc910.callPackage
    ({ mkDerivation, base, ghc-lib-parser, hspec, hspec-discover, text
     }:
       mkDerivation {
         pname = "ghc-syntax-highlighter";
         version = "0.0.12.0";
         sha256 = "sha256-qZ1FkqZIeICogR5QTsILNWaZrnCvt6LLJQq66QnFdGA=";
         enableSeparateDataOutput = true;
         libraryHaskellDepends = [ base ghc-lib-parser text ];
         testHaskellDepends = [ base hspec text ];
         testToolDepends = [ hspec-discover ];
         description = "Syntax highlighter for Haskell using the lexer of GHC";
         license = lib.licenses.bsd3;
         hydraPlatforms = lib.platforms.none;
       }) {};

  # "ghc-lib-parser-948" = haskell.packages.ghc94.callPackage (
  #   {
  #     mkDerivation,
  #     alex,
  #     array,
  #     base,
  #     binary,
  #     bytestring,
  #     containers,
  #     deepseq,
  #     directory,
  #     exceptions,
  #     filepath,
  #     ghc-prim,
  #     happy,
  #     parsec,
  #     pretty,
  #     process,
  #     time,
  #     transformers,
  #     unix,
  #   }:
  #   mkDerivation {
  #     pname = "ghc-lib-parser";
  #     version = "9.4.8.20231111";
  #     sha256 = "sha256-ylPKq7KITlm75bR0dkiohAnjp3pIL1O+uZ2LVdx2I/8=";
  #     enableSeparateDataOutput = true;
  #     libraryHaskellDepends = [
  #       array
  #       base
  #       binary
  #       bytestring
  #       containers
  #       deepseq
  #       directory
  #       exceptions
  #       filepath
  #       ghc-prim
  #       parsec
  #       pretty
  #       process
  #       time
  #       transformers
  #       unix
  #     ];
  #     libraryToolDepends = [
  #       alex
  #       happy
  #     ];
  #     description = "The GHC API, decoupled from GHC versions";
  #     license = lib.licenses.bsd3;
  #   }
  # ) { };

  # "ghc-lib-parser-ex-948" = haskell.packages.ghc94.callPackage (
  #   {
  #     mkDerivation,
  #     base,
  #     bytestring,
  #     containers,
  #     directory,
  #     extra,
  #     filepath,
  #     tasty,
  #     tasty-hunit,
  #     uniplate,
  #   }:
  #   mkDerivation {
  #     pname = "ghc-lib-parser-ex";
  #     version = "9.4.0.0";
  #     sha256 = "sha256-zj/zOaCiWaGK6SHFR7NXMurKGlZ6M/uhp8ZcFslzGRs=";
  #     libraryHaskellDepends = [
  #       base
  #       bytestring
  #       containers
  #       ghc-lib-parser-948
  #       uniplate
  #     ];
  #     testHaskellDepends = [
  #       base
  #       directory
  #       extra
  #       filepath
  #       ghc-lib-parser-948
  #       tasty
  #       tasty-hunit
  #       uniplate
  #     ];
  #     description = "Algorithms on GHC parse trees";
  #     license = lib.licenses.bsd3;
  #     hydraPlatforms = lib.platforms.none;
  #   }
  # ) { };

  # "fourmolu_0_11_0_0" = haskell.packages.ghc94.callPackage (
  #   {
  #     mkDerivation,
  #     aeson,
  #     ansi-terminal,
  #     array,
  #     base,
  #     binary,
  #     bytestring,
  #     Cabal-syntax,
  #     containers,
  #     deepseq,
  #     Diff,
  #     directory,
  #     file-embed,
  #     filepath,
  #     hspec,
  #     hspec-discover,
  #     hspec-megaparsec,
  #     megaparsec,
  #     MemoTrie,
  #     mtl,
  #     optparse-applicative,
  #     path,
  #     path-io,
  #     pretty,
  #     process,
  #     QuickCheck,
  #     scientific,
  #     syb,
  #     temporary,
  #     text,
  #     th-env,
  #     yaml,
  #   }:
  #   mkDerivation {
  #     pname = "fourmolu";
  #     version = "0.11.0.0";
  #     sha256 = "sha256-ucWB8H0hg+sLZz8+EhLLFEphJuL+Yr40mR8rLfIgR8M=";
  #     isLibrary = true;
  #     isExecutable = true;
  #     libraryHaskellDepends = [
  #       aeson
  #       ansi-terminal
  #       array
  #       base
  #       binary
  #       bytestring
  #       Cabal-syntax
  #       containers
  #       deepseq
  #       Diff
  #       directory
  #       file-embed
  #       filepath
  #       ghc-lib-parser-948
  #       megaparsec
  #       MemoTrie
  #       mtl
  #       scientific
  #       syb
  #       text
  #       yaml
  #     ];
  #     executableHaskellDepends = [
  #       base
  #       Cabal-syntax
  #       containers
  #       directory
  #       filepath
  #       ghc-lib-parser-948
  #       optparse-applicative
  #       text
  #       th-env
  #       yaml
  #     ];
  #     testHaskellDepends = [
  #       base
  #       bytestring
  #       Cabal-syntax
  #       containers
  #       Diff
  #       directory
  #       filepath
  #       ghc-lib-parser-948
  #       hspec
  #       hspec-megaparsec
  #       megaparsec
  #       path
  #       path-io
  #       pretty
  #       process
  #       QuickCheck
  #       temporary
  #       text
  #       yaml
  #     ];
  #     testToolDepends = [ hspec-discover ];
  #     description = "A formatter for Haskell source code";
  #     license = lib.licenses.bsd3;
  #     hydraPlatforms = lib.platforms.none;
  #     mainProgram = "fourmolu";
  #   }
  # ) { };

  # "hlint_3_5" = haskell.packages.ghc94.callPackage (
  #   {
  #     mkDerivation,
  #     aeson,
  #     ansi-terminal,
  #     base,
  #     bytestring,
  #     cmdargs,
  #     containers,
  #     cpphs,
  #     data-default,
  #     deriving-aeson,
  #     directory,
  #     extra,
  #     file-embed,
  #     filepath,
  #     filepattern,
  #     ghc-lib-parser,
  #     ghc-lib-parser-ex,
  #     hscolour,
  #     process,
  #     refact,
  #     text,
  #     transformers,
  #     uniplate,
  #     unordered-containers,
  #     utf8-string,
  #     vector,
  #     yaml,
  #   }:
  #   mkDerivation {
  #     pname = "hlint";
  #     version = "3.5";
  #     sha256 = "sha256-mL0SChCghsF9a/EXalENwSs2WB5akB8eAkVVuzzOrU8=";
  #     isLibrary = true;
  #     isExecutable = true;
  #     enableSeparateDataOutput = true;
  #     libraryHaskellDepends = [
  #       aeson
  #       ansi-terminal
  #       base
  #       bytestring
  #       cmdargs
  #       containers
  #       cpphs
  #       data-default
  #       deriving-aeson
  #       directory
  #       extra
  #       file-embed
  #       filepath
  #       filepattern
  #       ghc-lib-parser
  #       ghc-lib-parser-ex
  #       hscolour
  #       process
  #       refact
  #       text
  #       transformers
  #       uniplate
  #       unordered-containers
  #       utf8-string
  #       vector
  #       yaml
  #     ];
  #     executableHaskellDepends = [ base ];
  #     description = "Source code suggestions";
  #     license = lib.licenses.bsd3;
  #     hydraPlatforms = lib.platforms.none;
  #     mainProgram = "hlint";
  #     maintainers = [ lib.maintainers.maralorn ];
  #   }
  # ) { };

in

{
  # ghc94 = haskell.packages.ghc94.override {
  #   overrides = self: super: {
  #     ghc-parser = self.callCabal2nix "ghc-parser" (
  #       runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
  #     ) {};

  #     ipython-kernel = self.callCabal2nix "ipython-kernel" (
  #       runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
  #     ) {};

  #     ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

  #     zeromq4-haskell = super.zeromq4-haskell.overrideAttrs (oldAttrs: {
  #       buildInputs = oldAttrs.buildInputs ++ [libsodium];
  #     });

  #     ghc-lib-parser = ghc-lib-parser-948;
  #     ghc-lib-parser-ex = ghc-lib-parser-ex-948;

  #     ghc-syntax-highlighter = ghc-syntax-highlighter_0_0_9_0;

  #     hlint = hlint_3_5;

  #     ormolu = super.ormolu_0_5_2_0;

  #     optparse-applicative = super.optparse-applicative_0_15_1_0;

  #     fourmolu = fourmolu_0_11_0_0;
  #   };
  # };

  ghc96 = haskell.packages.ghc96.override {
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

  ghc98 = haskell.packages.ghc98.override {
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

  ghc910 = haskell.packages.ghc910.override {
    overrides = self: super: {
      ghc-parser = self.callCabal2nix "ghc-parser" (
        runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out"
      ) {};

      ipython-kernel = self.callCabal2nix "ipython-kernel" (
        runCommand "ipython-kernel" {} "cp -r ${ihaskell-source}/ipython-kernel $out"
      ) {};

      ihaskell = self.callCabal2nixWithOptions "ihaskell" ihaskell-source "--no-check" {};

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
