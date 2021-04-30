{ callPackage, writeText, generators, python, packageSelector ? (_: []) }:

rec {
  name = "python";

  binaries = [
    (shared.pythonWithPip packageSelector)
    python.pkgs.ipython
  ];

  # Without this, Python thinks its stdout and stderpr should have ASCII encoding ("ANSI_X3.4-1968")
  # Env = [
  #   "PYTHONIOENCODING=utf_8"
  # ];

  kernel = shared.kernel "Python 3.8" "python" ["python" "python3"] packageSelector;

  shared = callPackage ./shared.nix {
    pythonPackages = python.pkgs.override {
      overrides = self: super: {
        ipython = python.pkgs.ipython.overridePythonAttrs (old: { permitUserSite = true; });
      };
    };
    python = python;
  };

  modeInfo = callPackage ./mode_info.nix {};

  packageManager = callPackage ./package_manager/package_manager.nix { python = python; name = "python38"; displayName = "Python 3.8"; };

  languageServer = writeText "language_servers.yaml" (generators.toYAML {} [
    # Primary language server
    (callPackage ./language_server_jedi/config.nix {
      python = python;
      packages = packageSelector python.pkgs;
    }).config
    # (callPackage ./language_server_palantir/config.nix {
    #   python = python;
    #   packages = packageSelector python.pkgs;
    # }).config
    # (callPackage ./language_server_microsoft/config.nix {
    #   python = python;
    #   packages = packageSelector python.pkgs;
    # }).config

    # Secondary language servers (for diagnostics, formatting, etc.)
    (callPackage ./language_server_pylint/config.nix {
      python = python;
      packages = packageSelector python.pkgs;
    }).config
    (callPackage ./language_server_flake8/config.nix {
      python = python;
      packages = packageSelector python.pkgs;
    }).config
    (callPackage ./language_server_pycodestyle/config.nix {
      python = python;
      packages = packageSelector python.pkgs;
    }).config
  ]);

  homeFolderPaths = (import ../../util.nix).folderBuilder ./home_folder;

  extraGitIgnoreLines = [
    ".ipython"
  ];
}
