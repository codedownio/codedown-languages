with import <nixpkgs> {};

rec {
  listPackages = writeShellScriptBin "nix-list-packages.sh" ''
    QUERY="$1"
    LIMIT="$2"
    OFFSET="$3"
    if [ -z "$QUERY" ]; then
      echo "[]";
    else
      ${curl}/bin/curl https://clojars.org/search\?q\=$QUERY\&format\=json | ${jq}/bin/jq '.results' | ${jq}/bin/jq -Mc 'map(. + {"name": (.group_name + "/" + .jar_name)})'
    fi
  '';

  installPackage = writeShellScriptBin "clojure-install-package.sh" ''
    NAME="$1"
    VERSION="$2"

    ${leiningen}/bin/lein change :dependencies conj "[:$NAME, \"$version\"]"
  '';

  removePackage = writeShellScriptBin "clojure-remove-package.sh" ''
    NAME="$1"

    ${leiningen}/bin/lein
  '';

  packageManager = writeText "package_managers.yaml" (lib.generators.toYAML {} [{
    type = "generic";
    name = "clojure";
    displayName = "Clojure";
    attr = "clojure";
    image = ./logo-64x64.png;
    isAvailable = "exit 0";
    listAll = ''${listPackages}/bin/clojure-list-packages.sh'';
    listInstalled = "echo '[]'";
    install = ''${installPackage}/bin/clojure-install-package.sh'';
    remove = ''${removePackage}/bin/clojure-remove-package.sh'';
  }]);
}
