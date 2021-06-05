{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  language = "r";

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

  baseByName = name: lib.findSingle (x: x.name == name) null "multiple" baseOptions;

  packageOptions = base@{...}: {};

  languageServerOptions = base@{...}: packages: {};
}
