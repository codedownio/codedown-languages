{ lib
, jupyter-kernel
, snapshot
, ihaskell
, ghc
, callPackage

, attrs
, displayName
, extensions
, language ? lib.head attrs

# Needed for ihaskell's PATH
, gcc
}:

with lib;

let
  common = callPackage ../common.nix {};

  repls = [{
    display_name = "GHCi";
    attr = "ghci";
    proc = "${ghc.out}/bin/ghci";
  }];

  # Note: can actually get libdir by calling "ghc --print-libdir"
  libDir = if builtins.compareVersions ghc.version "9.6" < 0
    then "${ghc.out}/lib/${ghc.meta.name}"
    else "${ghc.out}/lib/${ghc.meta.name}/lib";

in

common.makeJupyterKernel (
  listToAttrs [{
    name = head attrs;
    value = {
      inherit displayName;
      argv = [
        "${ihaskell}/bin/ihaskell"
        "kernel"
        "{connection_file}"
        "-l" libDir
        "--html-code-wrapper-class" "cm-s-hite"
        "--html-code-token-prefix" ""
        "+RTS" "-M3g" "-N2" "-RTS"
      ];
      codemirror_mode = "haskell";
      inherit language;
      logo32 = ./haskell-logo-32x32.png;
      logo64 = ./haskell-logo-64x64.png;
      inherit repls;
      env = {
        PATH = lib.makeBinPath [ghc.out gcc.out];
      };
      metadata = {
        codedown = {
          inherit attrs extensions repls;
          language_version = "GHC " + ghc.version;
          priority = 1;
        };
      };
    };
  }]
)
