{ lib
, callPackage
, stdenv
, symlinkJoin
, makeWrapper

, haskell
, haskell-language-server
, haskell-nix
, filterToValid ? false
, ltsOnly ? true
}:

with callPackage ./inputs.nix {};

with lib;

let
  common = callPackage ../common.nix {};

  maximumByVersion = list: foldl (x: y: if common.lexicographyVersionNumber x.version > common.lexicographyVersionNumber y.version then x else y) (head list) list;

  compilers = zipAttrsWith (name: values: maximumByVersion values) (mapAttrsToList (name: value:
    let
      snapshot = (getAttr name haskell-nix.stackage) haskell-nix.hackage;
    in
      listToAttrs [(nameValuePair snapshot.compiler.nix-name (applyVersionToSnapshot name snapshot))]
  ) baseSnapshots);

  snapshotToCompiler = mapAttrs (name: value: ((getAttr name haskell-nix.stackage) haskell-nix.hackage).compiler.nix-name) baseSnapshots;

  getVersion = name: if hasPrefix "lts-" name then "b." + (removePrefix "lts-" name)
                     else if hasPrefix "nightly-" name then "a." + (removePrefix "nightly-" name)
                     else name;

  applyVersionToSnapshot = name: snapshot: let
    version = getVersion name; in snapshot // { inherit version; };

  # Filter to LTS only to speed up evaluation time
  baseSnapshots = (lib.filterAttrs (n: v: lib.hasInfix "lts" n) haskell-nix.snapshots);
  # baseSnapshots = haskell-nix.snapshots;

  validSnapshots = mapAttrs applyVersionToSnapshot
    (if filterToValid then (filterAttrs (n: v: hasAttr (getAttr n snapshotToCompiler) haskell.packages) baseSnapshots) else baseSnapshots);

  repls = ghc: {
    ghci = {
      display_name = "GHCi " + ghc.version;
      args = ["${ghc}/bin/ghci"];
      icon = ./haskell-logo-64x64.png;
    };
  };

in

compilers

  # listToAttrs (mapAttrsToList (name: snapshot:
  #   let displayName = "Haskell (Stackage " + name + ")";
  #       meta = {
  #         baseName = "haskell-stackage-" + name;
  #         name = "haskell-stackage-" + name;
  #         description = "An advanced, purely functional programming language";
  #         version = snapshot.version;
  #         inherit displayName;
  #         icon = ./haskell-logo-64x64.png;
  #       };

  #   in {
  #     name = meta.baseName;
  #     value = rec {
  #       packageOptions = snapshot;

  #       # Grab the meta from the library component
  #       # Could also search over other components?
  #       packageSearch = common.searcher (mapAttrs (name: value:
  #         let meta = (attrByPath ["components" "library" "meta"] null value); in
  #         if meta == null then value else value // { inherit meta; }) packageOptions);

  #       languageServerOptions = allLanguageServerOptions (snapshot.ghcWithPackages (ps: [])) "haskell";
  #       languageServerSearch = common.searcher languageServerOptions;

  #       settingsSchema = [];
  #       defaultSettings = {};

  #       build = args@{
  #         packages ? []
  #         , languageServers ? []
  #         , attrs ? [meta.baseName "haskell"]
  #         , extensions ? ["hs"]
  #         , settings ? defaultSettings
  #         , metaOnly ? false
  #       }:
  #         let
  #           settingsToUse = defaultSettings // settings;
  #           ghc = snapshot.ghcWithPackages (ps: (map (x: builtins.getAttr x ps) packages));

  #         in symlinkJoin {
  #           name = meta.baseName;

  #           paths = [
  #             (callPackage ./kernel.nix {
  #               inherit displayName attrs extensions metaOnly snapshot;
  #               ihaskell = callPackage ./ihaskell.nix {
  #                 inherit packages snapshot;
  #                 compiler = getAttr (getAttr name snapshotToCompiler) haskell.packages;
  #               };
  #               # enableVariableInspector = settingsToUse.enableVariableInspector;
  #             })

  #             (callPackage ./mode_info.nix { inherit attrs extensions; })
  #           ]
  #           ++ (if metaOnly then [] else [ghc])
  #           ++ (if metaOnly then [] else (map (y: builtins.getAttr y (allLanguageServerOptions ghc meta.baseName)) languageServers))
  #           ;

  #           passthru = {
  #             args = args // { baseName = meta.baseName; };
  #             settings = settingsToUse;
  #             inherit meta languageServerOptions packageOptions settingsSchema;
  #             repls = repls ghc;
  #           };
  #         };

  #       inherit meta;
  #     };
  #   }
  # ) validSnapshots)
