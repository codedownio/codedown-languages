{ compiler
, lib
, fetchFromGitHub
, nix-gitignore
, haskell
}:

let
  src = fetchFromGitHub {
    owner = "gibiansky";
    repo = "IHaskell";
    rev = "7e69c7e176ba234d29a57fcdff8d1bba04e34ab4";
    sha256 = "0ry2l1g5z04n5l3991p361fs9andbr7disiwamx09sbx0cn7wwm2";
  };

  # src = nix-gitignore.gitignoreSource
  #   [ "**/*.ipynb" "**/*.nix" "**/*.yaml" "**/*.yml" "**/\.*" "/Dockerfile" "/README.md" "/cabal.project" "/images" "/notebooks" "/requirements.txt" ] src';

  displays = self: builtins.listToAttrs (
    map
      (display: { name = "ihaskell-${display}"; value = self.callCabal2nix display "${src}/ihaskell-display/ihaskell-${display}" {}; })
      [ "aeson" "blaze" "charts" "diagrams" "gnuplot" "graphviz" "hatex" "juicypixels" "magic" "plot" "rlangqq" "static-canvas" "widgets" ]);

  ihaskellOverlay = (self: super: {
    ihaskell = haskell.lib.overrideCabal (self.callCabal2nix "ihaskell" src {}) (_drv: {
      preCheck = ''
        export HOME=$TMPDIR/home
        export PATH=$PWD/dist/build/ihaskell:$PATH
        export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
      '';
    });
    ghc-parser = self.callCabal2nix "ghc-parser" (src + /ghc-parser) {};
    ipython-kernel = self.callCabal2nix "ipython-kernel" (src + /ipython-kernel) {};
  } // displays self);

  haskellPackages = compiler.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) ihaskellOverlay;
  });

in

haskellPackages.ihaskell
