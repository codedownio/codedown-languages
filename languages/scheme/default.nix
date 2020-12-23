let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;

rec {
  name = "scheme";

  binaries = [mitscheme];

  kernel = jupyter-kernel.create {
    definitions = {
      scheme = {
        displayName = "MIT Scheme";
        argv = [
          "${import ./kernel.nix}/bin/mit_scheme_kernel"
          "{connection_file}"
        ];
        language = "scheme";
        logo32 = null;
        logo64 = ./logo-64x64.png;
      };
    };
  };

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "scheme";
    codeMirrorMode = "scheme";
    extensionsToHighlight = ["scm" "ss"];
    extensionsToRun = ["scm" "ss"];
  }]);
}
