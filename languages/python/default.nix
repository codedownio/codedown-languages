{ pkgs
, lib
, callPackage
, symlinkJoin
, stdenv
}:

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = pythonWithPackages: kernelName:
    let
      basePython = pythonWithPackages (_: []);
    in
      {
        # Primary language servers
        jedi = (callPackage ./language_server_jedi/config.nix { inherit pythonWithPackages kernelName; });
        pyright = (callPackage ./language_server_pyright/config.nix { inherit pythonWithPackages kernelName; });

        # Secondary language servers (for diagnostics, formatting, etc.)
        pylint = (callPackage ./language_server_pylint/config.nix { inherit pythonWithPackages kernelName; });
        flake8 = (callPackage ./language_server_flake8/config.nix { inherit pythonWithPackages kernelName; });
        pycodestyle = (callPackage ./language_server_pycodestyle/config.nix { inherit pythonWithPackages kernelName; });
      } // (lib.optionalAttrs (lib.hasAttr "python-language-server" pkgs) {
        microsoft = callPackage ./language_server_microsoft/config.nix { inherit pythonWithPackages kernelName; };
      }) // (lib.optionalAttrs ((lib.hasAttr "python-lsp-server" basePython.pkgs) && (lib.versionAtLeast basePython.pythonVersion "3.7")) {
        python-lsp-server = callPackage ./language_server_pythonlsp/config.nix { inherit pythonWithPackages kernelName; };
      }) // (lib.optionalAttrs (lib.hasAttr "python-language-server" basePython.pkgs) {
        python-language-server = callPackage ./language_server_palantir/config.nix { inherit pythonWithPackages kernelName; };
      });

  repls = python: {
    ipython = {
      display_name = "IPython " + python.pkgs.ipython.version;
      args = ["${python}/bin/ipython"];
      icon = ./logo-64x64.png;
    };
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

      settingsSchema = [
        {
          target = "permitUserSite";
          title = "Permit user site-packages";
          description = "Skip setting the PYTHONNOUSERSITE variable. This will allow your Python code to import local packages (e.g. from ~/.local/lib). This is useful if you want to use pip to install Python packages independently of Nix.";
          type = "boolean";
          defaultValue = false;
        }
        {
          target = "enableVariableInspector";
          title = "Enable variable inspector";
          description = "Enable the variable inspector, which will fetch runtime values of variables to show in the variables list.";
          type = "boolean";
          defaultValue = true;
        }
      ];

      meta = basePython.meta // {
        baseName = x;
        inherit displayName settingsSchema;
        version = basePython.version;
        icon = ./logo-64x64.png;
      };

  in {
    name = x;
    value = rec {
      packageOptions = basePython.pkgs;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = allLanguageServerOptions basePython.withPackages "python";
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [x "python"]
        , extensions ? ["py"]
        , settings ? {}
        , metaOnly ? false
      }:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
          ps = packageOptions;
          allPackages = [ps.ipykernel ps.ipywidgets]
                        ++ (map (x: builtins.getAttr x ps) packages);
          python = basePython.withPackages (_: allPackages);
          pythonWithPackages = f: basePython.withPackages (_: allPackages ++ f ps);

        in symlinkJoin {
          name = x;

          paths = [
            (callPackage ./kernel.nix {
              inherit python displayName attrs extensions metaOnly;
              enableVariableInspector = settingsToUse.enableVariableInspector;
            })

            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [python ps.ipython])
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y (allLanguageServerOptions pythonWithPackages x)) languageServers))
          ;

          passthru = {
            inherit meta packageOptions languageServerOptions settingsSchema;
            args = args // { baseName = x; };
            settings = settingsToUse;
            repls = repls python;
          };
        };

      inherit meta;
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
