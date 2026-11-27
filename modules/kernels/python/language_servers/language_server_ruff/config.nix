{ lib
, callPackage

, ruff

, kernelName
, attrs
}:


let
  common = callPackage ../../../common.nix {};

  languageServerName = "ruff";

  passthru = {
    inherit languageServerName;
    formatters = ["ruff"];
  };

in

common.writeTextDirWithMetaAndPassthru ruff.meta passthru "lib/codedown/language-servers/python-${kernelName}-ruff.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = ruff.version;
  display_name = "Ruff";
  description = ruff.meta.description;

  # From https://github.com/astral-sh/ruff/blob/main/docs/assets/ruff-favicon.png
  # under MIT License
  icon = ./icon_scaled_64x64.png;
  icon_monochrome = ./icon_scaled_64x64_monochrome.png;

  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  inherit attrs;
  type = "stream";
  args = ["${ruff}/bin/ruff" "server"];

  initialization_options = {
    settings = {
      configurationPreference = "filesystemFirst";
      lint.ignore = ["E402"];
      showSyntaxErrors = false;
    };
  };

  language_id = "python";
}])
