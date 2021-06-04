{ pkgs }:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../common.nix {};

in

rec {
  language = "python";

  baseOptions = let
    baseCandidates = [
      "python"
      "python2" "python27"
      "python3" "python36" "python37" "python38" "python39"
      "pypy"
      "pypy2" "pypy27"
      "pypy3" "pypy36" "pypy37" "pypy38" "pypy39"
    ];
  in
    map (x:
      let python = getAttr x pkgs; in {
            inherit python;
            name = x;
            meta = python.meta // {
              displayName = "Python " + python.version;
              logo = ./logo-64x64.png;
            };
          }
    ) (filter (x: hasAttr x pkgs) baseCandidates);

  baseByName = name: lib.findSingle (x: x.name == name) null "multiple" baseOptions;

  packageOptions = base@{python, ...}: python.pkgs.override {
    overrides = self: super: {
      ipython = python.pkgs.ipython.overridePythonAttrs (old: { permitUserSite = true; });
    };
  };

  settingsSchema = {
    permitUserSite = {
      title = "Show execution count";
      description = "Show the execution counter next to each code cell";
      type = "boolean";
      defaultValue = false;
    };
  };

  packageSearch = base@{python, ...}: common.searcher python.pkgs;

  languageServerOptions = base@{python, ...}: packages: {
    # Primary language server
    jedi = (callPackage ./language_server_jedi/config.nix {
      python = python;
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
