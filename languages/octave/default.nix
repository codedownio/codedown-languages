{ lib
, pkgs

, callPackage
, jupyter-kernel
, python3
, runCommand
, symlinkJoin
, writeTextDir
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "octave"
  ];

  settingsSchema = [];

  repls = octave: version: {
    octave = {
      display_name = "Octave " + version;
      attr = "octave";
      args = ["${octave}/bin/octave"];
      icon = ./octave-logo-64x64.png;
    };
  };

in

with lib;

listToAttrs (map (x:
  let
    baseOctave = getAttr x pkgs;

    meta = baseOctave.meta // {
      baseName = x;
      displayName = "Octave " + baseOctave.version;
      version = baseOctave.version;
      icon = ./octave-logo-64x64.png;
      inherit settingsSchema;
    };

    packageOptions = baseOctave.pkgs;
    packageSearch = common.searcher packageOptions;
    versions = {
      octave = baseOctave.version;
    };

  in {
    name = x;
    value = lib.makeOverridable ({
      packages ? []
      , settings ? {}
      , extraJupyterConfig ? null
      , attrs ? ["octave"]
      , extensions ? ["m"]
    }@args:
      let
        octaveComplete = baseOctave.override {
          python3 = python3;
        };

        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;

        octaveWithPackages = if lib.hasAttr "withPackages" octaveComplete
                             then
                               let chosenPackages = map (x: lib.getAttr x octaveComplete.pkgs) packages; in
                               if chosenPackages == [] then octaveComplete else octaveComplete.withPackages (ps: chosenPackages)
                             else octaveComplete;

        chosenPackages = if lib.hasAttr "pkgs" baseOctave
                         then baseOctave.pkgs
                         else [];

        # Wrapper derivation that only has "octave" and "octave-cli" binaries,
        # perfect for including in binaries and passing to the kernel
        octave = callPackage ./octave.nix { octaveComplete = octaveWithPackages; };

      in symlinkJoin {
        name = "octave";
        paths = [
          (callPackage ./kernel.nix {
            inherit octave extraJupyterConfig attrs extensions;
            version = baseOctave.version;
          })
          octave
        ];
        passthru = {
          inherit meta packageOptions packageSearch versions;
          args = args // { baseName = x; };
          repls = repls octaveWithPackages baseOctave.version;
          inherit settingsSchema settings;
          modes = {
            inherit attrs extensions;
            code_mirror_mode = "octave";
          };
        };
      }
    ) {};
  })
  (filter (x: hasAttr x pkgs) baseCandidates)
)

  # extraGitIgnoreLines = [
  #   ".octaverc"
  #   ".octave_hist"
  # ];


  # Solution for problem that was arising when Octave calls makeinfo from the texinfo
  # package, for example when running the command help('magic'). We were getting a perl
  # warning about failing to set the locale.
  # https://github.com/NixOS/nixpkgs/issues/38991
  # Env = [
  #   ''LOCALE_ARCHIVE_2_11=${glibcLocales}/lib/locale/locale-archive''
  #   ''LOCALE_ARCHIVE_2_27=${glibcLocales}/lib/locale/locale-archive''
  #   ''LOCALE_ARCHIVE=/usr/bin/locale''
  # ];
