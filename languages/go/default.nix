with import <nixpkgs> {};
with pythonPackages;
with buildGoPackage;

rec {
  name = "go";

  binaries = [go];

  kernel = jupyter-kernel.create {
    definitions = {
      go = {
        displayName = "Go";
        argv = [
          "${import ./gophernotes.nix}/bin/gophernotes"
          "{connection_file}"
        ];
        language = "go";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  modeInfo = writeTextDir "lib/codedown/go-modes.yaml" (lib.generators.toYAML {} [{
    attr_name = "go";
    code_mirror_mode = "go";
    extensions_to_highlight = ["go"];
    extensions_to_run = ["go"];
  }]);

  languageServer = writeTextDir "lib/codedown/go-language-servers.yaml" (lib.generators.toYAML {} [{
    name = "go";
    extensions = ["go"];
    attrs = ["go"];
    type = "stream";
    args = ["${import ./language_server.nix}/bin/go-langserver" "start"];
    initialization_options = {
      gocodeCompletionEnabled = true;
      lintTool = "golint";
    };
  }]);
}
