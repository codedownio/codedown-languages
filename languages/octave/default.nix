with import <nixpkgs> {};

let metakernel = import ../../metakernel;

in

rec {
  name = "octave";

  # TODO: trim this down
  binaries = [kernel octaveWithBinaries glibcLocales gcc cmake binutils binutils-unwrapped gnumake stdenv gfortran];

  # Solution for problem that was arising when Octave calls makeinfo from the texinfo
  # package, for example when running the command help('magic'). We were getting a perl
  # warning about failing to set the locale.
  # https://github.com/NixOS/nixpkgs/issues/38991
  # Env = [
  #   ''LOCALE_ARCHIVE_2_11=${glibcLocales}/lib/locale/locale-archive''
  #   ''LOCALE_ARCHIVE_2_27=${glibcLocales}/lib/locale/locale-archive''
  #   ''LOCALE_ARCHIVE=/usr/bin/locale''
  # ];

  kernel = let
    pythonWithOctaveCLI = runCommand "python" {
      python = python.buildEnv.override {
        extraLibs = [ (import ./kernel.nix) octaveWithBinaries ];
        # ignoreCollisions because of https://github.com/NixOS/nixpkgs/issues/22319
        ignoreCollisions = true;
      };
      octave = octaveWithBinaries;
      buildInputs = [python makeWrapper];
    } ''
    mkdir -p $out/bin/
    makeWrapper $python/bin/python $out/bin/python --suffix PATH ":" $octave/bin
  '';
  in
    jupyter-kernel.create {
      definitions = {
        octave = {
          displayName = "Octave";
          argv = [
            "${pythonWithOctaveCLI}/bin/python"
            "-m"
            "octave_kernel"
            "-f"
            "{connection_file}"
          ];
          language = "octave";
          logo32 = ./logo-32x32.png;
          logo64 = ./logo-64x64.png;
          metadata = {
            codedown = {
              priority = 1;
            };
          };
        };
      };
    };

  packageManager = callPackage ./package_manager.nix {};

  octaveWithBinaries = import ./octave.nix;

  homeFolderPaths = (import ../../util.nix).folderBuilder ./home_folder;

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
}
