with import <nixpkgs> {};
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
        logo64 = ./scheme-logo-64x64.png;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  modeInfo = writeTextDir "lib/codedown/modes/scheme.yaml" (lib.generators.toYAML {} [{
    attr_name = "scheme";
    code_mirror_mode = "scheme";
    extensions_to_highlight = ["scm" "ss"];
    extensions_to_run = ["scm" "ss"];
  }]);
}
