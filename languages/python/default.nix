with import <nixpkgs> {};
with stdenv.lib;

rec {
  name = "python";

  binaries = [shared.pythonWithPip];

  # Without this, Python thinks its stdout and stderr should have ASCII encoding ("ANSI_X3.4-1968")
  # Env = [
  #   "PYTHONIOENCODING=utf_8"
  # ];

  kernel = shared.kernel "Python 3.8" "python" ["python" "python3"];

  shared = callPackage ./shared.nix {
    pythonPackages = python3.pkgs.override {
      overrides = self: super: {
        ipython = python3.pkgs.ipython.overridePythonAttrs (old: { permitUserSite = true; });
      };
    };
    python = python3;
  };

  modeInfo = callPackage ./mode_info.nix {};

  packageManager = callPackage ./package_manager/package_manager.nix { python = python3; name = "python38"; displayName = "Python 3.8"; };

  languageServer = writeText "language_servers.yaml" (generators.toYAML {} [
    # Primary language server
    (callPackage ./language_server_jedi/config.nix { python = python3; }).config
    # (callPackage ./language_server_palantir/config.nix { python = python3; }).config
    # (callPackage ./language_server_microsoft/config.nix { python = python3; }).config

    # Secondary language servers (for diagnostics, formatting, etc.)
    (callPackage ./language_server_pylint/config.nix { python = python3; }).config
    (callPackage ./language_server_flake8/config.nix { python = python3; }).config
    (callPackage ./language_server_pycodestyle/config.nix { python = python3; }).config
  ]);

  homeFolderPaths = (import ../../util.nix).folderBuilder ./home_folder;

  extraGitIgnoreLines = [
    ".ipython"
  ];
}
