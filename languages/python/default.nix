{ pkgs
, lib
, callPackage
, symlinkJoin
, stdenv
}:

let
  common = callPackage ../common.nix {};

  chooseLanguageServers = settings: pythonWithPackages: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.jedi.enable") [(callPackage ./language_server_jedi/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.pyright.enable") [(callPackage ./language_server_pyright/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.pylint.enable") [(callPackage ./language_server_pylint/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.flake8.enable") [(callPackage ./language_server_flake8/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.pycodestyle.enable") [(callPackage ./language_server_pycodestyle/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.microsoft.enable") [(callPackage ./language_server_microsoft/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.python-lsp-server.enable") [(callPackage ./language_server_pythonlsp/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.python-language-server.enable") [(callPackage ./language_server_palantir/config.nix { inherit pythonWithPackages kernelName; })]
    ;

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
        {
          target = "lsp.jedi.enable";
          title = "Enable Jedi language server";
          type = "boolean";
          defaultValue = true;
        }
        {
          target = "lsp.pyright.enable";
          title = "Enable Pyright language server";
          type = "boolean";
          defaultValue = false;
        }
        {
          target = "lsp.pylint.enable";
          title = "Enable Pyright language server";
          type = "boolean";
          defaultValue = false;
        }
        {
          target = "lsp.flake8.enable";
          title = "Enable Flake8 language server";
          type = "boolean";
          defaultValue = false;
        }
        {
          target = "lsp.pycodestyle.enable";
          title = "Enable pycodestyle language server";
          type = "boolean";
          defaultValue = false;
        }
        {
          target = "lsp.pylint.enable";
          title = "Enable Pylint language server";
          type = "boolean";
          defaultValue = false;
        }
      ] ++ lib.optionals (lib.hasAttr "python-language-server" pkgs) [
        {
          target = "lsp.microsoft.enable";
          title = "Enable Microsoft Python language server";
          type = "boolean";
          defaultValue = false;
        }
      ] ++ lib.optionals ((lib.hasAttr "python-lsp-server" basePython.pkgs) && (lib.versionAtLeast basePython.pythonVersion "3.7")) [
        {
          target = "lsp.pythonlsp.enable";
          title = "Enable python-lsp-server language server";
          type = "boolean";
          defaultValue = false;
        }
      ] ++ lib.optionals (lib.hasAttr "python-language-server" basePython.pkgs) [
        {
          target = "lsp.pythonlsp.enable";
          title = "Enable python-language-server language server";
          type = "boolean";
          defaultValue = false;
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

      build = args@{
        packages ? []
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
          ++ (if metaOnly then [] else chooseLanguageServers settingsToUse pythonWithPackages x)
          ;

          passthru = {
            inherit meta packageOptions settingsSchema;
            args = args // { baseName = x; };
            settings = settingsToUse;
            repls = repls python;
          };
        };

      inherit meta;
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
