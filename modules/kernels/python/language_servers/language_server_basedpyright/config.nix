{ lib
, callPackage

, pythonWithPackages
, basedpyright

, kernelName
, attrs
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

  initialization_options = {
    "basedpyright.disableLanguageServices" = false;
    "basedpyright.disableOrganizeImports" = false;
    "python.analysis.autoImportCompletions" = true;
    "python.analysis.autoSearchPaths" = true;
    "python.analysis.diagnosticMode" = "openFilesOnly";
    "python.analysis.useLibraryCodeForTypes" = true;

    # No typeCheckingMode here: basedpyright only picks that up from a workspace/configuration
    # pull, not from initializationOptions (measured -- off/standard/recommended give 0/3/5
    # diagnostics over the pull, and no change at all through here). It runs at its own
    # default, "recommended", until codedown can serve it configuration.
    "python.pythonPath" = "${pythonEnv}/bin/python";
  };

  language_id = "python";
}])
