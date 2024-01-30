with import <nixpkgs> {};
with beamPackages;

rec {
  name = "elixir";

  kernel = jupyter-kernel.create {
    definitions = {
      python2 = {
        displayName = "Elixir";
        argv = [
          "${import ./kernel.nix}/bin/ielixir"
          "-f"
          "{connection_file}"
        ];
        language = "python2";
        logo32 = null;
        logo64 = null;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  languageServer = writeTextDir "lib/codedown/language-servers/elixir.yaml" (lib.generators.toYAML {} [{
      name = "elixir";
      extensions = ["exs"];
      type = "stream";
      args = [];
    }]);

  modeInfo = writeTextDir "lib/codedown/modes/elixir.yaml" (lib.generators.toYAML {} [{
    attr_name = "elixir";
    code_mirror_mode = "erlang";
    extensions_to_highlight = ["exs"];
    extensions_to_run = ["exs"];
  }]);
}
