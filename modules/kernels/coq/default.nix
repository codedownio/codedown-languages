{ callPackage
, lib
, symlinkJoin

, coqPackages
, coq-kernel

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  coq = coqPackages.coq;

  displayName = "Coq " + coq.version;

  isRocq = lib.versionAtLeast coq.version "9.0";

  packageOptions = coqPackages;
  packageSearch = common.searcher packageOptions;

in

symlinkJoin {
  name = coq.name;

  paths = [
    (callPackage ./kernel.nix {
      inherit coq displayName attrs extensions isRocq;
      # enableVariableInspector = settings.enableVariableInspector;
      chosenPackages = (
        map (x: builtins.getAttr x packageOptions) packages
      ) ++ lib.optionals isRocq [ packageOptions.stdlib ];
    })

    coq
  ]
  ++ []
  ;

  passthru = {
    meta = coq.meta // {
      inherit displayName settingsSchema;
      version = coq.version;
      icon = "${coq-kernel.logos}/logo-64x64.png";
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      coq = coq.version;
    };
    inherit settings settingsSchema;
    repls = {};
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "coq";
    };
  };
}
