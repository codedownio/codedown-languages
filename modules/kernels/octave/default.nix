{ lib
, pkgs

, callPackage
, jupyter-kernel
, python3
, runCommand
, symlinkJoin
, writeTextDir

, packages
, attrs
, extensions
, octave
, settings
, settingsSchema
}:

with lib;

let
  common = callPackage ../common.nix {};

  packageOptions = octave.pkgs;
  packageSearch = common.searcher packageOptions;

  octaveComplete = octave.override {
    python3 = python3;
  };

  octaveWithPackages = if lib.hasAttr "withPackages" octaveComplete
                       then
                         let chosenPackages = map (x: lib.getAttr x octaveComplete.pkgs) packages; in
                         if chosenPackages == [] then octaveComplete else octaveComplete.withPackages (ps: chosenPackages)
                       else octaveComplete;

  chosenPackages = if lib.hasAttr "pkgs" octave
                   then octave.pkgs
                   else [];

  # Wrapper derivation that only has "octave" and "octave-cli" binaries,
  # perfect for including in binaries and passing to the kernel
  octaveToUse = callPackage ./octave.nix { octaveComplete = octaveWithPackages; };

in

symlinkJoin {
  name = "octave";
  paths = [
    (callPackage ./kernel.nix {
      inherit attrs extensions;
      inherit (settings) extraJupyterConfig;
      octave = octaveToUse;
      version = octave.version;
    })
    octaveToUse
  ];
  passthru = {
    args = {
      inherit attrs extensions settings;
      packages = [];
    };
    meta = octave.meta // {
      baseName = "octave";
      displayName = "Octave " + octave.version;
      version = octave.version;
      icon = ./octave-logo-64x64.png;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    versions = {
      octave = octave.version;
    };
    inherit packageOptions packageSearch;
    repls = {
      octave = {
        display_name = "Octave " + octave.version;
        attr = "octave";
        args = ["${octaveWithPackages}/bin/octave"];
        icon = ./octave-logo-64x64.png;
      };
    };
    inherit settingsSchema settings;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "octave";
    };
  };
}
