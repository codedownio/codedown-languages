let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;
with stdenv;

rec {
  name = "rust";

  binaries = [rustc cargo gcc];

  kernel = jupyter-kernel.create {
    definitions = {
      rust = {
        displayName = "Rust";
        argv = [
          "${evcxr}/bin/evcxr_jupyter"
          "--control_file"
          "{connection_file}"
        ];
        language = "rust";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
      };
    };
  };

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "rust";
    extensions = ["rs"];
    attrs = ["rust"];
    type = "stream";
    args = ["${rustPackages.rls}/bin/rls"];
    notebook_suffix = ".rs";
  }]);

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "rust";
    codeMirrorMode = "rust";
    extensionsToHighlight = ["rs" "rc"];
    extensionsToRun = ["rs"];
  }]);
}
