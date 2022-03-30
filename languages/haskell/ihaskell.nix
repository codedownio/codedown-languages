{ compiler
, lib
, pkgs
, fetchFromGitHub
, nix-gitignore
, haskell
, buildEnv
, makeWrapper
, runCommand
, packages
, systemPackages ? (_: [])
}:

let
  src = fetchFromGitHub {
    owner = "gibiansky";
    repo = "IHaskell";
    rev = "7e69c7e176ba234d29a57fcdff8d1bba04e34ab4";
    sha256 = "0ry2l1g5z04n5l3991p361fs9andbr7disiwamx09sbx0cn7wwm2";
  };

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

  ihaskellEnv = haskellPackages.ghcWithPackages (ps: [ps.ihaskell] ++ (map (x: builtins.getAttr x ps) packages));

in

buildEnv {
  name = "ihaskell-with-packages";
  nativeBuildInputs = [ makeWrapper ];
  paths = [ ihaskellEnv ];
  postBuild = ''
    ls -lh $out/bin

    if [[ -L "$out/bin" ]]; then
      mv "$out/bin" $out/binlink
      mkdir -p $out/bin
      cp -r $out/binlink/* $out/bin
    fi

    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        echo "TRYING TO WRAP PROGRAM: $prg"
        wrapProgram $prg \
          --prefix PATH : "${lib.makeBinPath ([ihaskellEnv] ++ (systemPackages pkgs))}"
      fi
    done
  '';
}
