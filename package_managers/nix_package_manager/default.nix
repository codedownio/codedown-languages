with import <nixpkgs> {};
with lib.lists;

let
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

  yamlFile = writeText "package_managers.yaml" (lib.generators.toYAML {} [{
    type = "generic";
    name = "nix";
    displayName = "Nix";
    image = ./logo.png;
    description = ''[Nix](https://nixos.org/) is a package manager for Linux systems. Look here for general-purpose packages and libraries.'';
    isAvailable = "which nix && which nix-env";
    listAll = ''${listPackages}/bin/nix-list-packages.sh'';
    refreshPackageCache = ''nix search -u > /dev/null'';
    listInstalled = ''nix-env -q --json | ${jq}/bin/jq 'to_entries | map({"name": .value.pname, "version": .value.version, "description": .value.meta?.description?}) ' '';
    install = ''${installPackage}/bin/nix-install-package.sh'';
    remove = ''${removePackage}/bin/nix-remove-package.sh'';
  }]);

in

runCommand "codedown-nix-package-manager" {} ''
  mkdir -p $out/lib/codedown-nix-package-manager
  cd $out/lib/codedown-nix-package-manager
  ln -s ${yamlFile} package_managers.yaml
''
