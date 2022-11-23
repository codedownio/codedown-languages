{ pkgs
, lib
, callPackage
, symlinkJoin
, stdenv
}:

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = coq: kernelName: {};

  repls = coq: {};

  coq_jupyter = callPackage ./coq_jupyter {};

  baseCandidates = [
    "coqPackages"
    "coqPackages_8_5"
    "coqPackages_8_6"
    "coqPackages_8_7"
    "coqPackages_8_8"
    "coqPackages_8_9"
    "coqPackages_8_10"
    "coqPackages_8_11"
    "coqPackages_8_12"
    "coqPackages_8_13"
    "coqPackages_8_14"
    "coqPackages_8_15"
    "coqPackages_8_16"
    "coqPackages_8_17"
    "coqPackages_8_18"
    "coqPackages_8_19"
    "coqPackages_8_20"
  ];

  isLessCommon = candidate: !(lib.elem candidate ["coqPackages" "coqPackages_8_20"]);

in

lib.listToAttrs (map (x:
  let coqPackages = lib.getAttr x pkgs;
      baseCoq = coqPackages.coq;
      baseName = with builtins; (substring 0 3 x) + (substring 11 (stringLength x - 11) x);
      displayName = "Coq " + baseCoq.version;
      meta = baseCoq.meta // {
        inherit baseName displayName;
        version = baseCoq.version;
        icon = coq_jupyter.sizedLogo "64";
        lessCommon = isLessCommon x;
      };

  in {
    name = baseName;
    value = rec {
      packageOptions = coqPackages;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = allLanguageServerOptions baseCoq "coq";
      languageServerSearch = common.searcher languageServerOptions;

      settingsSchema = [];
      defaultSettings = {};

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [baseName "coq"]
        , extensions ? ["v"]
        , settings ? defaultSettings
        , metaOnly ? false
      }:
        let
          settingsToUse = defaultSettings // settings;

          ps = packageOptions;

          # coq = baseCoq.withPackages (_: [ps.ipykernel ps.ipywidgets] ++ (map (x: builtins.getAttr x ps) packages));
          coq = baseCoq;

        in symlinkJoin {
          name = baseName;

          paths = [
            (callPackage ./kernel.nix {
              inherit coq displayName attrs extensions metaOnly;
              enableVariableInspector = settingsToUse.enableVariableInspector;
            })

            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [
            coq
          ])
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y (allLanguageServerOptions coq baseName)) languageServers));

          passthru = {
            args = args // { inherit baseName; };
            settings = settingsToUse;
            repls = repls coq;
            inherit meta languageServerOptions packageOptions settingsSchema;
          };
        };

      inherit meta;
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
