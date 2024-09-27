{ pkgs
, lib
, callPackage
, symlinkJoin
, stdenv

, coqPackages

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  coq_jupyter = callPackage ./coq_jupyter {};

  coq = coqPackages.coq;

  displayName = "Coq";

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
    args = {
      inherit attrs extensions settings;
      packages = [];
    };
    repls = {};
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "coq";
    };
  };
}
