{ pkgs
, lib
, callPackage
, symlinkJoin
, stdenv
}:

let
  common = callPackage ../common.nix {};

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

  settingsSchema = [];

  chooseLanguageServers = settings: coq: kernelName:
    []
  ;

in

lib.listToAttrs (map (x:
  let
    coqPackages = lib.getAttr x pkgs;
    baseCoq = coqPackages.coq;
    baseName = with builtins; (substring 0 3 x) + (substring 11 (stringLength x - 11) x);
    displayName = "Coq";
    meta = baseCoq.meta // {
      inherit baseName displayName settingsSchema;
      version = baseCoq.version;
      icon = coq_jupyter.sizedLogo "64";
      lessCommon = isLessCommon x;
    };

    packageOptions = coqPackages;
    packageSearch = common.searcher packageOptions;
    versions = {
      coq = baseCoq.version;
    };

  in {
    name = baseName;
    value = lib.makeOverridable ({
      packages ? []
      , attrs ? [baseName "coq"]
      , extensions ? ["v"]
      , settings ? {}
    }@args:
      let
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;

        coq = baseCoq;

      in symlinkJoin {
        name = baseName;

        paths = [
          (callPackage ./kernel.nix {
            inherit coq displayName attrs extensions;
            enableVariableInspector = settingsToUse.enableVariableInspector;
            chosenPackages = map (x: builtins.getAttr x packageOptions) packages;
          })

          coq
        ]
        ++ (chooseLanguageServers settingsToUse coq baseName)
        ;

        passthru = {
          inherit meta packageOptions packageSearch versions;
          inherit settings settingsSchema;
          args = args // { inherit baseName; };
          repls = repls coq;
          modes = {
            inherit attrs extensions;
            code_mirror_mode = "coq";
          };
        };
      }
    ) {};
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
