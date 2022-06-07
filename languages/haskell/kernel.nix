{ lib
, callPackage
, runCommand
, makeWrapper

, jupyter-kernel
, snapshot
, attrs
, extensions
, displayName
, ihaskell
, ghc

, metaOnly ? false
}:

with lib;

let
  common = callPackage ../common.nix {};

  repls = [{
    display_name = "GHCi";
    proc = "${ghc.out}/bin/ghci";
  }];

  ihaskellWrapped = runCommand "ihaskell-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin

    for prg in ${ihaskell}/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        if [[ "$(basename $prg)" == "ihaskell" ]]; then
          makeWrapper $prg $out/bin/$(basename $prg) \
            --add-flags "kernel -l $(${ghc}/bin/ghc --print-libdir)" \
            --prefix PATH : "${lib.makeBinPath [ghc]}"
        else
          makeWrapper $prg $out/bin/$(basename $prg) $prg \
            --prefix PATH : "${lib.makeBinPath [ghc]}"
        fi
      fi
    done
  '';

in

common.makeJupyterKernelInner metaOnly (
  listToAttrs [{
    name = head attrs;
    value = {
      inherit displayName;
      argv = [
        "${ihaskellWrapped}/bin/ihaskell"
        "{connection_file}"
        "+RTS" "-M3g" "-N2" "-RTS"
      ];
      language = head attrs;
      logo32 = ./haskell-logo-32x32.png;
      logo64 = ./haskell-logo-64x64.png;
      inherit repls;
      metadata = {
        codedown = {
          inherit attrs extensions repls;
          priority = 1;
        };
      };
    };
  }]
)
