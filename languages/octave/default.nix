{pkgs, lib, callPackage, runCommand, writeText}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? baseName,
    otherLanguageKeys ? []
  }:
    let
      base = lib.findSingle (x: x.name  == baseName) null "multiple" metadata.baseOptions;

      # baseOctave = if lib.hasAttr "withPackages" base.octave
      #              then base.octave.withPackages (ps: packages ps)
      #              else base.octave;
      baseOctave = base.octave;

      packages = if lib.hasAttr "pkgs" base.octave
                 then base.octave.pkgs
                 else [];
      availableLanguageServers = metadata.languageServerOptions base packages;
    in rec {
      name = "octave";

      # Wrapper derivation that only has "octave" and "octave-cli" binaries,
      # perfect for including in binaries and passing to the kernel
      octave = callPackage ./octave.nix { octave = baseOctave; };

      binaries = [octave];
      kernel = callPackage ./kernel.nix { inherit octave; };
      extraGitIgnoreLines = [
        ".octaverc"
        ".octave_hist"
      ];
      modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
        attrName = "octave";
        codeMirrorMode = "octave";
        extensionsToHighlight = ["m"];
        extensionsToRun = ["m"];
      }]);
    };
}



  # Solution for problem that was arising when Octave calls makeinfo from the texinfo
  # package, for example when running the command help('magic'). We were getting a perl
  # warning about failing to set the locale.
  # https://github.com/NixOS/nixpkgs/issues/38991
  # Env = [
  #   ''LOCALE_ARCHIVE_2_11=${glibcLocales}/lib/locale/locale-archive''
  #   ''LOCALE_ARCHIVE_2_27=${glibcLocales}/lib/locale/locale-archive''
  #   ''LOCALE_ARCHIVE=/usr/bin/locale''
  # ];
