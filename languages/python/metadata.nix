{ pkgs }:

with pkgs;

rec {
  name = "python";

  displayName = python: "Python " + python.version;

  baseOptions = {
    python2 = let python = python2; in { inherit python; displayName = displayName python; meta = python.meta; logo = ./logo-64x64.png; };
    python3 = let python = python27; in { inherit python; displayName = displayName python; meta = python.meta; logo = ./logo-64x64.png; };
    python27 = let python = python27; in { inherit python; displayName = displayName python; meta = python.meta; logo = ./logo-64x64.png; };
    python36 = let python = python36; in { inherit python; displayName = displayName python; meta = python.meta; logo = ./logo-64x64.png; };
    python37 = let python = python37; in { inherit python; displayName = displayName python; meta = python.meta; logo = ./logo-64x64.png; };
    python38 = let python = python38; in { inherit python; displayName = displayName python; meta = python.meta; logo = ./logo-64x64.png; };
    python39 = let python = python39; in { inherit python; displayName = displayName python; meta = python.meta; logo = ./logo-64x64.png; };
  };
  
  packageOptions = base@{python, ...}: python.pkgs.override {
    overrides = self: super: {
      ipython = python.pkgs.ipython.overridePythonAttrs (old: { permitUserSite = true; });
    };
  };

  languageServerOptions = base@{python, ...}: packages: {
    # Primary language server
    jedi = (callPackage ./language_server_jedi/config.nix {
      python = python;
      packages = packages;
    });
    palantir = (callPackage ./language_server_palantir/config.nix {
      python = python;
      packages = packages;
    });
    microsoft = (callPackage ./language_server_microsoft/config.nix {
      python = python;
      packages = packages;
    });

    # Secondary language servers (for diagnostics, formatting, etc.)
    pylint = (callPackage ./language_server_pylint/config.nix {
      python = python;
      packages = packages;
    });
    flake8 = (callPackage ./language_server_flake8/config.nix {
      python = python;
      packages = packages;
    });
    pycodestyle = (callPackage ./language_server_pycodestyle/config.nix {
      python = python;
      packages = packages;
    });
  };
}
