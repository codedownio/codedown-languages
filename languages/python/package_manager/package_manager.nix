{stdenv, pkgs, python, name, displayName}:

with pkgs;
with stdenv.lib;

let
  shared = callPackage ../shared.nix { inherit python; pythonPackages = python.pkgs; };

  manylinux1 = callPackage ../manylinux1.nix {};

  pythonWithReqParser = python.buildEnv.override {
    extraLibs = let ps = python.pkgs; in [(shared.pipNoUserSite ps) requirements-parser ps.setuptools];
    permitUserSite = true;
    makeWrapperArgs = [
      # Append libs needed at runtime for manylinux1 compliance
      "--set" "LD_LIBRARY_PATH" (makeLibraryPath manylinux1.libs)

      # Include the location of built-in packages as an environment variable
      "--set" "SYSTEM_PACKAGES_JSON" ''${builtInPackagesJSON}''

      # We need to be able to call uname and possibly lsb_release
      "--prefix" "PATH" ":" "${pkgs.coreutils}/bin"
      "--prefix" "PATH" ":" "${pkgs.lsb-release}/bin"
    ];
  };

  writePy = writers.makeScriptWriter {
    interpreter = "${pythonWithReqParser}/bin/python";
  };

  requirements-parser = python.pkgs.buildPythonPackage rec {
    pname = "requirements-parser";
    version = "0.2.0";

    # doCheck = false;
    checkInputs = [ python.pkgs.nose ];

    src = python.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "1m2fgnyrh4vb5canm7cp30b04f7vh8869z6kb2gsw19dbj4ywqsr";
    };
  };

  listPackages = writePy "python-list-packages.py" ./python_list_packages.py;

  builtInPackagesJSON = writeText "python-builtin-packages.json" (lib.generators.toJSON {} (map (pkg: {
    name = pkg.pname;
    version = pkg.version;
    system = true;
  }) [])); # (shared.defaultPackages python.pkgs)

  listInstalled = writePy "python-list-installed-packages.py" ./python_list_installed_packages.py;

  installPackage = writePy "python-install-package.py" ./python_install_package.py;

  removePackage = writePy "python-remove-package.sh" ./python_remove_package.py;

in

writeText "package_managers.yaml" (lib.generators.toYAML {} [{
  type = "generic";
  name = name;
  displayName = displayName;
  attr = "python";
  image = ../logo-64x64.png;
  description = ''The Python package manager keeps top-level requirements in `top-requirements.txt`, and the full set of dependencies in `requirements.txt`.'';
  isAvailable = "which pip";
  listAll = ''${listPackages}'';
  refreshPackageCache = ''exit 0'';
  listInstalled = ''${listInstalled}'';
  install = ''${installPackage}'';
  remove = ''${removePackage}'';
}])
