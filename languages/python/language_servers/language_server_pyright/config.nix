{ lib
, callPackage
, makeWrapper
, runCommand
, writeTextDir

, pythonWithPackages
, pyright

, kernelName
}:


let
  common = callPackage ../../../common.nix {};

  pythonEnv = pythonWithPackages (_: []);

  pyrightWrapped = runCommand "pyright-withenv" { inherit pythonEnv; buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${pyright}/bin/pyright-langserver $out/bin/pyright-langserver \
      --set PYTHONPATH $pythonEnv/${pythonEnv.sitePackages}
  '';

in

common.writeTextDirWithMeta pyright.meta "lib/codedown/language-servers/python-${kernelName}-pyright.yaml" (lib.generators.toYAML {} [{
  name = "pyright";
  display_name = "Pyright";
  description = pyright.meta.description;

  # Obtained from https://raw.githubusercontent.com/microsoft/pyright/30b467e75c6a032c8e0973ee85edf9db268826a7/client/images/pyright-icon.png
  # under MIT License
  icon = ./icon_scaled_64x64.png;

  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  attrs = ["python"];
  type = "stream";
  args = ["${pyright}/bin/pyright-langserver" "--stdio"];

  # Taken from https://github.com/emacs-lsp/lsp-pyright/blob/master/lsp-pyright.el
  initialization_options = {
    "pyright.disableLanguageServices" = true;
    "pyright.disableOrganizeImports" = true;
    "python.analysis.autoImportCompletions" = true;
    "python.analysis.typeshedPaths" = true;
    "python.analysis.stubPath" = true;
    "python.analysis.useLibraryCodeForTypes" = true;
    "python.analysis.diagnosticMode" = "openFilesOnly";
    "python.analysis.typeCheckingMode" = "basic";
    "python.analysis.logLevel" = "Trace"; # Information
    "python.analysis.autoSearchPaths" = true;
    "python.analysis.extraPaths" = ["/home/user"];
    "python.pythonPath" = "${pythonEnv}/bin/python";
    # We need to send empty string, otherwise  pyright-langserver fails with parse error
    # "python.venvPath" = "";
  };
}])
