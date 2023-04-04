{ lib
, pkgs
, fetchFromGitHub
, nix-gitignore
, haskell
, buildEnv
, makeWrapper
, runCommand
, packages
, snapshot
, systemPackages ? (_: [])
}:

let
  src = fetchFromGitHub {
    owner = "gibiansky";
    repo = "IHaskell";
    rev = "8afa4e22c5724da89fec85a599ee129ab5b4cb9a";
    sha256 = "0rkvqrpnsyp33x8mzh1v48vm96bpmza14nl6ah1sgjfbp86ihi8p";
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

  extendedSnapshot = (lib.makeExtensible (_: snapshot)).extend (old: super: {
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) ihaskellOverlay;
  });

  ihaskellEnv = extendedSnapshot.ghcWithPackages (ps: [ps.ihaskell] ++ (map (x: builtins.getAttr x ps) packages));

in

buildEnv {
  name = "ihaskell-with-packages";
  nativeBuildInputs = [ makeWrapper ];
  paths = [ ihaskellEnv extendedSnapshot.ihaskell.components.exes.ihaskell  ];
  postBuild = ''
    if [[ -L "$out/bin" ]]; then
      mv "$out/bin" $out/binlink
      mkdir -p $out/bin
      cp -r $out/binlink/* $out/bin
    fi

    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        if [[ "$(basename $prg)" == "ihaskell" ]]; then
          wrapProgram $prg \
            --add-flags "kernel -l $(${ihaskellEnv}/bin/ghc --print-libdir)" \
            --prefix PATH : "${lib.makeBinPath ([ihaskellEnv] ++ (systemPackages pkgs))}"
        else
          wrapProgram $prg \
            --prefix PATH : "${lib.makeBinPath ([ihaskellEnv] ++ (systemPackages pkgs))}"
        fi
      fi
    done
  '';
}
