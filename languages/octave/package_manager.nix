{stdenv, pkgs}:

with pkgs;
with pkgs.lib;

let
  writePy = writers.makeScriptWriter {
    interpreter = "${python37}/bin/python";
  };

  listPackages = writePy "octave-list-packages.py" (readFile ./list_all_packages.py);

  listInstalled = writeShellScriptBin "octave-list-installed-packages.sh" ''
    ${octave}/bin/octave ${./list_installed_packages.m}
  '';

  installPackage = writeShellScriptBin "octave-install-package.sh" ''
    echo "Installing package: $1"
    ${octave}/bin/octave --eval "pkg install -forge $1"
  '';

  removePackage = writeShellScriptBin "octave-remove-package.sh" ''
    ${octave}/bin/octave --eval "pkg uninstall $1"
  '';

in

writeText "package_managers.yaml" (lib.generators.toYAML {} [{
  type = "generic";
  name = "octave";
  displayName = "Octave";
  attr = "octave";
  image = ./logo-64x64.png;
  description = ''The Octave package manager installs packages from Octave Forge.'';
  isAvailable = "which octave";
  listAll = ''${listPackages}'';
  refreshPackageCache = ''exit 0'';
  listInstalled = ''${listInstalled}/bin/octave-list-installed-packages.sh'';
  install = ''${installPackage}/bin/octave-install-package.sh'';
  remove = ''${removePackage}/bin/octave-remove-package.sh'';
}])
