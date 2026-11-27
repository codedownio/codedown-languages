{ lib
, callPackage

, pythonWithPackages
, pyrefly

, kernelName
, attrs
, settings
}:


let
  common = callPackage ../../../common.nix {};

  pythonEnv = pythonWithPackages (_: []);

  languageServerName = "pyrefly";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru pyrefly.meta passthru "lib/codedown/language-servers/python-${kernelName}-pyrefly.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = pyrefly.version;
  display_name = "Pyrefly";
  description = pyrefly.meta.description;

  # From https://github.com/facebook/pyrefly/blob/main/website/static/img/Pyrefly-Symbol.svg
  # under MIT License
  icon = ./icon_scaled_64x64.png;
  icon_monochrome = ./icon_scaled_64x64_monochrome.png;

  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  inherit attrs;
  type = "stream";
  args = ["${pyrefly}/bin/pyrefly" "lsp"];
  env.PATH = "${pythonEnv}/bin";

  # Keyed by the section name pyrefly requests over workspace/configuration.
  initialization_options = {
    python = {
      pythonPath = "${pythonEnv}/bin/python";
      analysis.diagnosticMode = "openFilesOnly";
      pyrefly = {
        inherit (settings) disableLanguageServices typeCheckingMode;
        extraPaths = ["/home/user"];
      };
    };
  };

  language_id = "python";
}])
