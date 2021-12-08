{ lib
, callPackage
, writeText
, writeTextDir
, stdenv
, symlinkJoin
, fetchFromGitHub
}:

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = snapshot: kernelName: {

  };

  haskellNix = import (fetchFromGitHub {
    owner = "input-output-hk";
    repo = "haskell.nix";
    rev = "d231ee71dc511806ff75a6d83c7481fa25bbf8fe";
    sha256 = "07jnklzcki5m2lz5bv4yllgmwkwyplcsvbhfr92dgv9g8dlnwbg1";
  }) {};

  # This must be chosen to match haskellNix.sources.nixpkgs!
  # We do it ourselves because we want to use fetchFromGitHub instead of fetchTarball.
  nixpkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "110a2c9ebbf5d4a94486854f18a37a938cfacbbb";
    sha256 = "0v12ylqxy1kl06dgln6h5k8vhlfzp8xvdymljj7bl0avr0nrgrcm";
  }) haskellNix.nixpkgsArgs;

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

      # Grab the meta from the library component
      # Could also search over other components?
      packageSearch = common.searcher (lib.mapAttrs (name: value:
        let meta = (lib.attrByPath ["components" "library" "meta"] null value); in
        if meta == null then value else value // { inherit meta; }) packageOptions);

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
