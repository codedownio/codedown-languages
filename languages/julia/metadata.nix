{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseCandidates = [
    "julia"
    "julia1_3"
    "julia1_5"
    "julia1_6"
  ];
  baseOptions = map (x:
    let julia = getAttr x pkgs; in {
      inherit julia;
      name = x;
      meta = julia.meta // {
        displayName = "Julia " + julia.version;
        logo = ./logo-64x64.png;
      };
    }
  ) (filter (x: (hasAttr x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates);

  packageOptions = base@{julia, ...}: [];

  languageServerOptions = base@{julia, ...}: packages: {

  };
}
