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

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
      name = "elixir";
      extensions = ["exs"];
      type = "stream";
      args = [];
    }]);

  modeInfo = writeTextDir "lib/codedown/elixir-mode-config.yaml" (lib.generators.toYAML {} [{
    attrName = "elixir";
    codeMirrorMode = "erlang";
    extensionsToHighlight = ["exs"];
    extensionsToRun = ["exs"];
  }]);
}
