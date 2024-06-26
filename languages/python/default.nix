{ callPackage
, lib
, pkgs
, poetry2nix
, python-language-server
, pyright
, symlinkJoin
, stdenv
}:

let
  common = callPackage ../common.nix {};

  hasPythonLspServer = python: (lib.hasAttr "python-lsp-server" python.pkgs) && (lib.versionAtLeast python.pythonVersion "3.7");
  hasPythonLanguageServer = python: lib.hasAttr "python-language-server" python.pkgs;

  chooseLanguageServers = settings: pythonWithPackages: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.jedi.enable") [(callPackage ./language_servers/language_server_jedi/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.pyright.enable") [(callPackage ./language_servers/language_server_pyright/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.pylint.enable") [(callPackage ./language_servers/language_server_pylint/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.flake8.enable") [(callPackage ./language_servers/language_server_flake8/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.pycodestyle.enable") [(callPackage ./language_servers/language_server_pycodestyle/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.microsoft.enable") [(callPackage ./language_servers/language_server_microsoft/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.python-lsp-server.enable" && (hasPythonLspServer (pythonWithPackages (ps: [])))) [(callPackage ./language_servers/language_server_pythonlsp/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (common.isTrue settings "lsp.python-language-server.enable" && (hasPythonLanguageServer (pythonWithPackages (ps: [])))) [(callPackage ./language_servers/language_server_palantir/config.nix { inherit pythonWithPackages kernelName; })]
  ;

  repls = python: {
    ipython = {
      display_name = "IPython " + python.pkgs.ipython.version;
      attr = "ipython";
      args = ["${python}/bin/ipython"];
      icon = ./python-logo-64x64.png;
    };
  };

  # Fine-grained candidates
  # These don't work for now because Nixpkgs only keeps one set of Python packages,
  # aimed at the currently supported Python.
  # baseCandidates = [
  #   "python"
  #   "python2" "python27"
  #   "python3" "python36" "python37" "python38" "python39" "python310" "python311" "python312" "python313" "python314" "python315"

  #   "pypy"
  #   "pypy2" "pypy27"
  #   "pypy3" "pypy36" "pypy37" "pypy38" "pypy39" "pypy310" "pypy311" "pypy312" "pypy313" "pypy314" "pypy315"
  # ];

  baseCandidates = [
    "python"
    "python3"

    # Reached EOL: Nixpkgs won't build without complaints
    # "python2"

    # Currently pypy3 nose marked as broken
    # "pypy"
    # "pypy2"
    # "pypy3"
  ];

  # Pythons that don't work with the ipykernel, ipywidgets, etc. of the Nixpkgs Python package set,
  # so we have a special package checked in under ./envs
  specialEnvPythons = {
    "python38" = pkgs.python38;
    "python39" = pkgs.python39;
    "python312" = pkgs.python312;
  };

in

lib.listToAttrs (map (x:
  let
    basePython =
      if lib.hasAttr x specialEnvPythons then (poetry2nix.mkPoetryEnv {
        projectDir = ./envs/${x};
        python = specialEnvPythons.${x};
        overrides = import ./envs/${x}/poetry-overrides.nix { inherit poetry2nix; };
      }).overrideAttrs (_: { version = specialEnvPythons.${x}.version; })
      else lib.getAttr x pkgs;

    displayName = "Python " +  basePython.version;

    settingsSchema = [
      {
        title = "Language servers";
        level = 1;
        type = "heading";
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
        title = "Enable Pylint language server";
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
    ] ++ lib.optionals (lib.hasAttr "python-language-server" pkgs) [
      {
        target = "lsp.microsoft.enable";
        title = "Enable Microsoft Python language server";
        type = "boolean";
        defaultValue = false;
      }
    ] ++ lib.optionals (hasPythonLspServer basePython) [
      {
        target = "lsp.python-lsp-server.enable";
        title = "Enable python-lsp-server language server";
        type = "boolean";
        defaultValue = false;
      }
    ] ++ lib.optionals (hasPythonLanguageServer basePython) [
      {
        target = "lsp.python-language-server.enable";
        title = "Enable python-language-server language server";
        type = "boolean";
        defaultValue = false;
      }
    ] ++ [
      {
        title = "Miscellaneous";
        level = 1;
        type = "heading";
      }
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
      icon = ./python-logo-64x64.png;
    };

    packageOptions = basePython.pkgs;
    packageSearch = common.searcher packageOptions;
    versions = {
      python = basePython.version;
      jedi-language-server = basePython.pkgs.jedi-language-server.version;
      pyright = pyright.version;
      pylint = basePython.pkgs.pylint.version;
      flake8 = basePython.pkgs.flake8.version;
      pycodestyle = basePython.pkgs.pycodestyle.version;
    }
    // lib.optionalAttrs (hasPythonLspServer basePython) { python-lsp-server = basePython.pkgs.python-lsp-server.version; }
    ;

  in
    {
      name = x;
      value = lib.makeOverridable ({
        packages ? []
        , attrs ? [x "python"]
        , extensions ? ["py"]
        , settings ? {}
      }@args:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
          ps = packageOptions;
          chosenPackages = map (x: builtins.getAttr x ps) packages;
          allPackages = [ps.ipykernel ps.ipywidgets] ++ chosenPackages;
          python = basePython.withPackages (_: allPackages);
          pythonWithPackages = f: basePython.withPackages (_: allPackages ++ f ps);
          languageServers = chooseLanguageServers settingsToUse pythonWithPackages x;

        in symlinkJoin {
          name = x;

          paths = [
            (callPackage ./kernel.nix {
              inherit python displayName attrs extensions;
              enableVariableInspector = settingsToUse.enableVariableInspector;
            })

            python
            ps.ipython
          ]
          ++ languageServers
          ;

          passthru = {
            inherit meta packageOptions packageSearch versions;
            inherit settingsSchema settings;
            args = args // { baseName = x; };
            repls = repls python;
            modes = {
              inherit attrs extensions;
              code_mirror_mode = "python";
            };
            languageServerNames = map (x: x.languageServerName) languageServers;
          };
        }
      ) {};
    }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
