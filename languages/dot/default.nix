let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;

rec {
  name = "dot";

  binaries = [graphviz];

  kernel = let
    fontsConf = makeFontsConf {
      fontDirectories = [
        carlito dejavu_fonts
        freefont_ttf xorg.fontmiscmisc
        # liberation_ttf_v1_binary
        # liberation_ttf_v2_binary
      ];
    };
  in
    jupyter-kernel.create {
      definitions = {
        dot = {
          displayName = "Dot (Graphviz)";
          argv = [
            "${import ./kernel.nix}/bin/python"
            "-m"
            "dot_kernel"
            "-f"
            "{connection_file}"
          ];
          language = "dot";
          logo32 = ./logo-32x32.png;
          logo64 = ./logo-64x64.png;
          env = { FONTCONFIG_FILE = "${fontsConf}"; };
        };
      };
    };

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "dot";
    codeMirrorMode = "";
    codeMirrorMimeType = "text/plain";
    extensionsToHighlight = ["dot" "gv"];
    extensionsToRun = ["dot" "gv"];
  }]);
}
