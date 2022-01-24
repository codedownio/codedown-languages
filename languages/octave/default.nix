{ pkgs
, lib
, jupyter-kernel
, runCommand
, callPackage
, writeTextDir
, python3
, ghostscript
, gnuplot
, graphicsmagick
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "octave"
  ];

  repls = octave: version: {
    octave = {
      display_name = "Octave " + version;
      args = ["${octave}/bin/octave"];
      icon = ./logo-64x64.png;
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
      icon = ./logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = baseOctave.pkgs;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = {};
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , extraJupyterConfig ? null
        , attrs ? ["octave"]
        , extensions ? ["m"]
        , metaOnly ? false
      }:
        let
          octaveComplete = baseOctave.override {
            qscintilla = null;
            overridePlatforms = ["x86_64-linux" "x86_64-darwin"];
            gnuplot = gnuplot;
            ghostscript = ghostscript;
            graphicsmagick = graphicsmagick;
            python = python3;
          };

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
            (callPackage ./kernel.nix { inherit octave extraJupyterConfig attrs extensions; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
            octave
          ];
          passthru = {
            inherit meta packageOptions languageServerOptions;
            args = args // { baseName = x; };
            repls = repls octaveWithPackages baseOctave.version;
          };
        };

      inherit meta;
    };
  }
) (filter (x: hasAttr x pkgs) baseCandidates))

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
