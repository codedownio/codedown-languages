{pkgs, lib, callPackage, writeText}:

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
      python = base.python.withPackages (ps: [ps.ipykernel ps.ipywidgets] ++ (packages ps));
      availableLanguageServers = metadata.languageServerOptions base python.pkgs;
    in rec {
      name = "octave";
      octave = pkgs.octave.override {
        overridePlatforms = ["x86_64-linux" "x86_64-darwin"];
        gnuplot = pkgs.gnuplot;
        ghostscript = pkgs.ghostscript;
        graphicsmagick = pkgs.graphicsmagick;
        python = pkgs.python3;
      };
      octaveWithPackages = octave; #.withPackages (packages octave.pkgs);
      binaries = [octaveWithPackages];
      kernel = callPackage ./kernel.nix { octave = octaveWithPackages; };
      # packageManager = callPackage ./package_manager.nix {};
      # homeFolderPaths = (import ../../util.nix).folderBuilder ./home_folder;
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
