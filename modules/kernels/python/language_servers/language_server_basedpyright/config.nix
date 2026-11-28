{ lib
, callPackage

, pythonWithPackages
, basedpyright

, kernelName
, attrs
, settings
}:


let
  common = callPackage ../../../common.nix {};

  pythonEnv = pythonWithPackages (_: []);

  languageServerName = "basedpyright";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru basedpyright.meta passthru "lib/codedown/language-servers/python-${kernelName}-basedpyright.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = basedpyright.version;
  display_name = "basedpyright";
  description = basedpyright.meta.description;

  # From https://github.com/DetachHead/basedpyright/blob/main/packages/vscode-pyright/images/pyright-icon.png
  # under MIT License
  icon = ./icon_scaled_64x64.png;
  icon_monochrome = ./icon_scaled_64x64_monochrome.png;

  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  inherit attrs;
  type = "stream";
  args = ["${basedpyright}/bin/basedpyright-langserver" "--stdio"];
  env.PATH = "${pythonEnv}/bin";

  # Keyed by the section names basedpyright requests over workspace/configuration.
  initialization_options = {
    python.pythonPath = "${pythonEnv}/bin/python";
    basedpyright = {
      inherit (settings) disableLanguageServices;
      disableOrganizeImports = false;
      analysis = {
        inherit (settings) typeCheckingMode diagnosticSeverityOverrides;
        autoImportCompletions = true;
        autoSearchPaths = true;
        diagnosticMode = "openFilesOnly";
        extraPaths = ["/home/user"];
        useLibraryCodeForTypes = true;
      };
    };
  };

  language_id = "python";
}])
