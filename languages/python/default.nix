{ pkgs
, lib
, callPackage
, symlinkJoin
, stdenv
}:

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = python: {
    # Primary language server
    jedi = (callPackage ./language_server_jedi/config.nix { inherit python; });
    palantir = (callPackage ./language_server_palantir/config.nix { inherit python; });
    microsoft = (callPackage ./language_server_microsoft/config.nix { inherit python; });

    # Secondary language servers (for diagnostics, formatting, etc.)
    pylint = (callPackage ./language_server_pylint/config.nix { inherit python; });
    flake8 = (callPackage ./language_server_flake8/config.nix { inherit python; });
    pycodestyle = (callPackage ./language_server_pycodestyle/config.nix { inherit python; });
  };

  baseCandidates = [
    "python"
    "python2" "python27"
    "python3" "python36" "python37" "python38" "python39"
    "pypy"
    "pypy2" "pypy27"
    "pypy3" "pypy36" "pypy37" "pypy38" "pypy39"
  ];

in

lib.listToAttrs (map (x:
  let basePython = lib.getAttr x pkgs;
      displayName = "Python " + basePython.version;
      meta = basePython.meta // {
        baseName = x;
        inherit displayName;
        icon = ./logo-64x64.png;
      };

  in {
    name = x;
    value = rec {
      packageOptions = basePython.pkgs;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = allLanguageServerOptions basePython;
      languageServerSearch = common.searcher languageServerOptions;

      settingsSchema = [
        {
          target = "permitUserSite";
          title = "Permit user site-packages";
          description = "Skip setting the PYTHONNOUSERSITE variable. This will allow your Python code to import local packages (e.g. from ~/.local/lib). This is useful if you want to use pip to install Python packages independently of Nix.";
          type = "boolean";
          defaultValue = false;
        }
      ];
      defaultSettings = {
        permitUserSite = false;
      };

      build = args@{
        packages ? []
        , languageServers ? []
        , codeDownAttr ? "python"
        , otherLanguageKeys ? []
        , settings ? defaultSettings
      }:
        let
          ps = packageOptions.override {
            overrides = self: super: {
              ipython = basePython.pkgs.ipython.overridePythonAttrs (old: { permitUserSite = settings.permitUserSite; });
            };
          };
          python = basePython.withPackages (_: [ps.ipykernel ps.ipywidgets] ++ (map (x: builtins.getAttr x ps) packages));

        in symlinkJoin {
          name = x;

          paths = [
            python
            ps.ipython

            (callPackage ./kernel.nix {
              inherit python otherLanguageKeys displayName;
              codeDownAttr = codeDownAttr;
            })

            (callPackage ./mode_info.nix {})

            (allLanguageServerOptions python).jedi
            (allLanguageServerOptions python).pylint
          ];
          # ++ (map (x: builtins.getAttr x (allLanguageServerOptions python)) languageServers);

          passthru = {
            args = args // { baseName = x; };
            inherit meta languageServerOptions packageOptions settingsSchema settings;
          };
        };

      inherit meta;
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))


  # languageServer = writeTextDir "lib/codedown/python-language-servers.yaml" (pkgs.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
  # extraGitIgnoreLines = [".ipython"];
