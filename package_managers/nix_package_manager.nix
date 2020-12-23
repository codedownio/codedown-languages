let nixpkgs = import (import ../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with lib.lists;

rec {
  listPackages = writeShellScriptBin "nix-list-packages.sh" ''
    QUERY="$1"
    LIMIT="$2"
    OFFSET="$3"
    nix search "$QUERY" --json | ${jq}/bin/jq -Mc "to_entries | .[$OFFSET:] | [limit($LIMIT ; .[])] | map(.value + {\"name\": .key, \"display_name\": .value.pkgName} | del(.pkgName))"
  '';

  installPackage = writeShellScriptBin "nix-install-package.sh" ''
    nameToInstall="$1"

    # Echo the command being run for clarity
    echo "nix-env -iA $nameToInstall"

    nix-env -iA "$nameToInstall"
  '';

  removePackage = writeShellScriptBin "nix-remove-package.sh" ''
    # Echo the command being run for clarity
    echo "nix-env -e $1"

    nix-env -e "$1"
  '';

  packageManager = {
    config = writeText "package_managers.yaml" (lib.generators.toYAML {} [{
      type = "generic";
      name = "nix";
      displayName = "Nix";
      image = ./nix_package_manager/logo.png;
      description = ''[Nix](https://nixos.org/) is a package manager for Linux systems. Look here for general-purpose packages and libraries.'';
      isAvailable = "which nix && which nix-env";
      listAll = ''${listPackages}/bin/nix-list-packages.sh'';
      refreshPackageCache = ''nix search -u > /dev/null'';
      listInstalled = ''nix-env -q --json | ${jq}/bin/jq 'to_entries | map({"name": .value.pname, "version": .value.version, "description": .value.meta?.description?}) ' '';
      install = ''${installPackage}/bin/nix-install-package.sh'';
      remove = ''${removePackage}/bin/nix-remove-package.sh'';
    }]);
  };
}
