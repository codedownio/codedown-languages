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

rec {
  metadata = callPackage ./metadata.nix {};

  modeInfo = writeTextDir "lib/codedown/octave-modes.yaml" (lib.generators.toYAML {} [{
    attrName = "octave";
    codeMirrorMode = "octave";
    extensionsToHighlight = ["m"];
    extensionsToRun = ["m"];
  }]);

  build = args@{
    baseName
    , packages ? []
    , languageServers ? []
    , codeDownAttr ? baseName
    , otherLanguageKeys ? []
    , extraJupyterConfig ? null
  }:
    let
      base = metadata.baseByName baseName;

      octaveComplete = base.octave.override {
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

      chosenPackages = if lib.hasAttr "pkgs" base.octave
                       then base.octave.pkgs
                       else [];

      # Wrapper derivation that only has "octave" and "octave-cli" binaries,
      # perfect for including in binaries and passing to the kernel
      octave = callPackage ./octave.nix { octaveComplete = octaveWithPackages; };

      availableLanguageServers = metadata.languageServerOptions base chosenPackages;

    in symlinkJoin {
      name = "octave";
      paths = [
        (callPackage ./kernel.nix { inherit octave extraJupyterConfig; })
        octave
      ];
      passthru = {
        inherit args metadata;
        meta = base.meta;
      };
    };
}

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
