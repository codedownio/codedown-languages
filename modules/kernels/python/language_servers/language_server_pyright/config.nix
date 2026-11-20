{ lib
, callPackage
, pythonWithPackages
, pyright

, kernelName
, attrs
, settings
}:


let
  common = callPackage ../../../common.nix {};

  pythonEnv = pythonWithPackages (_: []);

  languageServerName = "pyright";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru pyright.meta passthru "lib/codedown/language-servers/python-${kernelName}-pyright.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = pyright.version;
  display_name = "Pyright";
  description = pyright.meta.description;

  # Obtained from https://raw.githubusercontent.com/microsoft/pyright/30b467e75c6a032c8e0973ee85edf9db268826a7/client/images/pyright-icon.png
  # under MIT License
  icon = ./icon_scaled_64x64.png;
  icon_monochrome = ./icon_scaled_64x64_monochrome.png;

  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  inherit attrs;
  type = "stream";
  args = ["${pyright}/bin/pyright-langserver" "--stdio"];
  env.PATH = "${pythonEnv}/bin";

  # Keyed by the section names pyright requests over workspace/configuration.
  initialization_options = {
    python = {
      pythonPath = "${pythonEnv}/bin/python";
      analysis = {
        inherit (settings) typeCheckingMode diagnosticSeverityOverrides;
        autoImportCompletions = true;
        autoSearchPaths = true;
        diagnosticMode = "openFilesOnly";
        extraPaths = ["/home/user"];
        useLibraryCodeForTypes = true;
      };
    };
    pyright = {
      inherit (settings) disableLanguageServices;
      disableOrganizeImports = true;
    };
  };

  language_id = "python";
}])
