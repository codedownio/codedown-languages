{ callPackage
, python312
, symlinkJoin

, coqPackages

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  coq = coqPackages.coq;

  displayName = "Coq " + coq.version;

  coq_jupyter = callPackage ./coq_jupyter {
    python3 = python312;
    inherit coq;
  };

  packageOptions = coqPackages;
  packageSearch = common.searcher packageOptions;

in

symlinkJoin {
  name = coq.name;

  paths = [
    (callPackage ./kernel.nix {
      inherit coq displayName attrs extensions;
      enableVariableInspector = settings.enableVariableInspector;
      chosenPackages = map (x: builtins.getAttr x packageOptions) packages;
      inherit coq_jupyter;
    })

    coq
  ]
  ++ []
  ;

  passthru = {
    meta = coq.meta // {
      inherit displayName settingsSchema;
      version = coq.version;
      icon = coq_jupyter.sizedLogo "64";
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
