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

  baseCandidates = [
    "coq"
    "coq_8_5"
    "coq_8_6"
    "coq_8_7"
    "coq_8_8"
    "coq_8_9"
    "coq_8_10"
    "coq_8_11"
    "coq_8_12"
    "coq_8_13"
    "coq_8_14"
    "coq_8_15"
    "coq_8_16"
    "coq_8_17"
    "coq_8_18"
    "coq_8_19"
    "coq_8_20"
  ];

in

lib.listToAttrs (map (x:
  let baseCoq = lib.getAttr x pkgs;
      displayName = "Coq " + baseCoq.version;
      meta = baseCoq.meta // {
        baseName = x;
        inherit displayName;
        version = baseCoq.version;
        icon = ./logo-64x64.png;
      };

  in {
    name = x;
    value = rec {
      packageOptions = baseCoq.pkgs;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = allLanguageServerOptions baseCoq "coq";
      languageServerSearch = common.searcher languageServerOptions;

      settingsSchema = [];
      defaultSettings = {};

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [x "coq"]
        , extensions ? ["py"]
        , settings ? defaultSettings
        , metaOnly ? false
      }:
        let
          settingsToUse = defaultSettings // settings;

          ps = packageOptions;

          # coq = baseCoq.withPackages (_: [ps.ipykernel ps.ipywidgets] ++ (map (x: builtins.getAttr x ps) packages));
          coq = baseCoq;

        in symlinkJoin {
          name = x;

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
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y (allLanguageServerOptions coq x)) languageServers));

          passthru = {
            args = args // { baseName = x; };
            settings = settingsToUse;
            repls = repls coq;
            inherit meta languageServerOptions packageOptions settingsSchema;
          };
        };

      inherit meta;
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
