{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseOptions = [{
    inherit R rWrapper rPackages;
    name = R.meta.name;
    meta = R.meta // {
      displayName = "R " + R.version;
      logo = ./logo-64x64.png;
    };
  }];

  packageOptions = base@{python, ...}: {};

  languageServerOptions = base@{python, ...}: packages: {};
}
