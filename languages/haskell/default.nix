{ lib
, callPackage
, writeText
, writeTextDir
, stdenv
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = snapshot: kernelName: {

  };

  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/d231ee71dc511806ff75a6d83c7481fa25bbf8fe.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;

in

lib.listToAttrs (lib.mapAttrsToList (name: snapshot:
  let displayName = "Haskell (Stackage " + name + ")";
      meta = {
        baseName = "haskell-stackage-" + name;
        name = "haskell-stackage-" + name;
        description = "Haskell haskell haskell";
        inherit displayName;
        icon = ./logo-64x64.svg;
      };

  in {
    name = meta.baseName;
    value = rec {
      packageOptions = snapshot;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = allLanguageServerOptions snapshot "haskell";
      languageServerSearch = common.searcher languageServerOptions;

      settingsSchema = [];
      defaultSettings = {};

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [meta.baseName "haskell"]
        , extensions ? ["hs"]
        , settings ? defaultSettings
        , metaOnly ? false
      }:
        let
          settingsToUse = defaultSettings // settings;

        in symlinkJoin {
          name = meta.baseName;

          paths = [
            # (callPackage ./kernel.nix {
            #   inherit displayName attrs extensions metaOnly snapshot;
            #   # enableVariableInspector = settingsToUse.enableVariableInspector;
            # })

            (snapshot.ghcWithPackages (ps: (map (x: builtins.getAttr x ps) packages)))

            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ];

          passthru = {
            args = args // { baseName = meta.baseName; };
            settings = settingsToUse;
            inherit meta languageServerOptions packageOptions settingsSchema;
          };
        };

      inherit meta;
    };
  }
) haskell.snapshots)


  # languageServer = writeTextDir "lib/codedown/python-language-servers.yaml" (pkgs.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
  # extraGitIgnoreLines = [".ipython"];


  # rec {
  #   # ihaskellWithPackages = ihaskell.override {
  #   #   ghcWithPackages = haskell.haskellPackages.ghcWithPackages (ps: with ps;
  #   #     [ lens conduit conduit-extra aeson ]
  #   #   );
  #   # };

  #   hls = callPackage ./hls.nix {};

  #   languageServer = writeTextDir "lib/codedown/haskell-language-servers.yaml" (pkgs.lib.generators.toYAML {} [hls]);

  #   extraGitIgnoreLines = [".stack"];
  # }
