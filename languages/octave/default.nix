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

  modeInfo = writeTextDir "lib/codedown/octave-modes.yaml" (lib.generators.toYAML {} [{
    attrName = "octave";
    codeMirrorMode = "octave";
    extensionsToHighlight = ["m"];
    extensionsToRun = ["m"];
  }]);

in

with lib;

listToAttrs (map (x:
  let
    baseOctave = getAttr x pkgs;
  in {
    name = x;
    value = rec {
      packageOptions = baseOctave.pkgs;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = {};

      build = args@{
        packages ? []
        , languageServers ? []
        , codeDownAttr ? "octave"
        , otherLanguageKeys ? []
        , extraJupyterConfig ? null
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
            (callPackage ./kernel.nix { inherit octave extraJupyterConfig; })
            octave
          ];
          passthru = {
            args = args // { baseName = x; };
            meta = baseOctave.meta;
            inherit packageOptions languageServerOptions;
          };
        };

      meta = baseOctave.meta // {
        baseName = x;
        displayName = "Octave " + octave.version;
        icon = ./logo-64x64.png;
      };
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
