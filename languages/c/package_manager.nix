let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with makeWrapper;
with writers;

rec {
  listInstalledCPackages = writePython3 "list_installed_c_packages.py" {} ''
    import json
    import os
    import sys

    results = []

    if os.path.isdir("deps"):
        for dir in os.listdir("deps"):
            dep = os.path.join("deps", dir)
            if not os.path.isdir(dep):
                continue

            packageFile = os.path.join(dep, "package.json")
            if not os.path.isfile(packageFile):
                continue

            try:
                with open(packageFile, 'r') as f:
                    contents = json.loads(f.read())
                    results.append({"name": contents["repo"],
                                    "version": contents["version"]})
            except Exception as e:
                print("Exception in list_installed_c_packages.py when parsing %s" %
                      packageFile, e, file=sys.stderr)

    print(json.dumps(results))
  '';

  installPackage = writeShellScriptBin "c-install-package.sh" ''
    echo "clib install \"$1\""
    ${clib}/bin/clib install "$1"
  '';

  removePackage = writePython3 "c-remove-package.py" {} ''
    import json
    import os
    import shutil
    import sys

    name = sys.argv[1]

    if os.path.exists("deps"):
        for dir in os.listdir("deps"):
            dep = os.path.join("deps", dir)
            if not os.path.isdir(dep):
                continue

            packageFile = os.path.join(dep, "package.json")
            if not os.path.isfile(packageFile):
                continue

            try:
                with open(packageFile, 'r') as f:
                    contents = json.loads(f.read())
                    if contents["repo"] == name:
                        print("rm -rf %s" % dep)
                        shutil.rmtree(dep)
                        sys.exit(0)
            except Exception as e:
                print("Error reading C package file %s" % packageFile, e,
                      file=sys.stderr)
                pass

    print("Couldn't find package %s on disk" % name)
    sys.exit(1)
  '';

  listAll = writeShellScriptBin "c-list-all-packages.sh" ''
    QUERY="$1"
    LIMIT="$2"
    OFFSET="$3"

    search_output=$(${clib}/bin/clib search "$QUERY" --json --no-color)
    echo "$search_output" | ${jq}/bin/jq ".[$OFFSET:] | [limit($LIMIT ; .[])] | map(. + {\"name\": .repo} | del(.repo))"
  '';

  packageManager = {
    config = writeText "package_managers.yaml" (lib.generators.toYAML {} [{
      type = "generic";
      name = "c";
      displayName = "C";
      attr = "c";
      image = ./c.png;
      isAvailable = "exit 0";
      listAll = ''${listAll}/bin/c-list-all-packages.sh'';
      listInstalled = "${listInstalledCPackages}";
      install = ''${installPackage}/bin/c-install-package.sh'';
      remove = ''${removePackage}'';
    }]);
  };
}
