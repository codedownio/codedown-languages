let nixpkgs = import (import ../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with nodePackages;

rec {
  name = "javascript";

  binaries = [npm];

  kernel = jupyter-kernel.create {
    definitions = {
      javascript = {
        displayName = "Javascript";
        language = "javascript";
        argv = [
          "${(callPackage ./ijavascript {}).ijavascript}/bin/ijskernel"
          "{connection_file}"
        ];
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
      };
    };
  };

  packageManager = writeText "package_managers.yaml" (lib.generators.toYAML {} [{
    type = "generic";
    name = "Javascript";
    attr = "javascript";
    isAvailable = "exit 0";
    listAll = "${npm}/bin/npm search --json {query}";
    listInstalled = ''${npm}/bin/npm list --depth 0 --json | ${jq}/bin/jq '.dependencies // {} | to_entries | map({"name": .key, "version": .value.version})' '';
    install = ["bash" "-c" ''(ls -d -- /home/user/packages.json > /dev/null 2>&1) || ${npm}/bin/npm init -y; ${npm}/bin/npm install --save {name}''];
    remove = ["bash" "-c" "${npm}/bin/npm uninstall --save {name}"];
  }]);

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "javascript";
    extensions = ["js" "jsx" "ts" "tsx"];
    type = "stream";
    args = ["${javascript-typescript-langserver}/bin/javascript-typescript-stdio"];
  }]);
}
