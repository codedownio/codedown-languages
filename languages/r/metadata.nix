{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseOptions = [{
    inherit R rWrapper rPackages;
    name = R.meta.name;
    meta = R.meta // {
      inherit language;
      baseName = R.meta.name;
      displayName = if hasAttr "version" R then "R " + R.version else "R";
      icon = ./logo-64x64.png;
    };
  }];

  packageOptions = base@{python, ...}: {};

  languageServerOptions = base@{python, ...}: packages: {};
}
