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
    owner = "IHaskell";
    repo = "IHaskell";
    rev = "c547ee2fdc0a09cf4129b19292147fec38527a55";
    sha256 = "lpUSdhZ2HtUMcUygG5ORbib9Dc9SE2e81fQHO0UWNCo=";
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
