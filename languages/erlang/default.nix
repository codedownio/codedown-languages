let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;

rec {
  name = "erlang";

  kernel = jupyter-kernel.create {
    definitions = {
      python2 = {
        displayName = "Erlang";
        argv = [
          "${executable/bin/todo}"
          "-f"
          "{connection_file}"
        ];
        language = "python2";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
      };
    };
  };

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "python";
    extensions = ["py"];
    type = "tcp";
    args = ["python_language_server" "--tcp" "--host=localhost" "--port={port_number}"];
  }]);
}
