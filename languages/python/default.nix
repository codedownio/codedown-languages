{ callPackage
, lib
, pkgs
, poetry2nix
, python-language-server
, pyright
, symlinkJoin
, stdenv

, python3

, packages
, attrs
, extensions
, settings
, settingsSchema
}:

let
  common = callPackage ../common.nix {};

  hasPythonLspServer = python: (lib.hasAttr "python-lsp-server" python.pkgs) && (lib.versionAtLeast python.pythonVersion "3.7");
  hasPythonLanguageServer = python: lib.hasAttr "python-language-server" python.pkgs;

  displayName = "Python " +  python3.version;
  kernelName = "python3";

  packageOptions = python3.pkgs;
  packageSearch = common.searcher packageOptions;
  allPackages =
    [packageOptions.ipykernel packageOptions.ipywidgets]
    ++ map (x: builtins.getAttr x packageOptions) (map common.packageName packages)
  ;

  pythonWithPackages = f: python3.withPackages (_: allPackages ++ f packageOptions);

  languageServers =
    []
    ++ lib.optionals settings.lsp.jedi.enable [(callPackage ./language_servers/language_server_jedi/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals settings.lsp.pyright.enable [(callPackage ./language_servers/language_server_pyright/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals settings.lsp.pylint.enable [(callPackage ./language_servers/language_server_pylint/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals settings.lsp.flake8.enable [(callPackage ./language_servers/language_server_flake8/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals settings.lsp.pycodestyle.enable [(callPackage ./language_servers/language_server_pycodestyle/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals settings.lsp.microsoft.enable [(callPackage ./language_servers/language_server_microsoft/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (settings.lsp.python-lsp-server.enable && (hasPythonLspServer (pythonWithPackages (ps: [])))) [(callPackage ./language_servers/language_server_pythonlsp/config.nix { inherit pythonWithPackages kernelName; })]
    ++ lib.optionals (settings.lsp.python-language-server.enable && (hasPythonLanguageServer (pythonWithPackages (ps: [])))) [(callPackage ./language_servers/language_server_palantir/config.nix { inherit pythonWithPackages kernelName; })]
  ;

  pythonToUse = python3.withPackages (_: allPackages);

in

symlinkJoin {
  name = kernelName;

  paths = [
    (callPackage ./kernel.nix {
      python = pythonToUse;
      inherit displayName attrs extensions;
      enableVariableInspector = settings.enableVariableInspector;
    })

    pythonToUse
    packageOptions.ipython
  ]
  ++ languageServers
  ;

  passthru = {
    meta = python3.meta // {
      baseName = kernelName;
      inherit displayName settingsSchema;
      version = python3.version;
      icon = ./python-logo-64x64.png;
    };
    inherit packageOptions packageSearch;
    versions = {
      python = python3.version;
      jedi-language-server = python3.pkgs.jedi-language-server.version;
      pyright = pyright.version;
      pylint = python3.pkgs.pylint.version;
      flake8 = python3.pkgs.flake8.version;
      pycodestyle = python3.pkgs.pycodestyle.version;
    }
    // lib.optionalAttrs (hasPythonLspServer python3) { python-lsp-server = python3.pkgs.python-lsp-server.version; }
    ;
    inherit settingsSchema settings;
    args = { inherit attrs extensions settings packages; };
    repls = {
      ipython = {
        display_name = "IPython " + pythonToUse.pkgs.ipython.version;
        attr = "ipython";
        args = ["${pythonToUse}/bin/ipython"];
        icon = ./python-logo-64x64.png;
      };
    };
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "python";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
