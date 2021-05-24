{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseOptions = [{
    inherit R rWrapper rPackages;
    name = x;
    displayName = "R " + R.version;
    meta = R.meta;
    logo = ./logo-64x64.png;
  }];

  packageOptions = base@{python, ...}: {};

  languageServerOptions = base@{python, ...}: packages: {};
}
