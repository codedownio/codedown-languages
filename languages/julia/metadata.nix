{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  language = "julia";

  baseOptions = let
    baseCandidates = [
      "julia"
      "julia1_3"
      "julia1_5"
      "julia1_6"
      "julia1_6-bin"
    ];
  in
    map (x:
      let julia = getAttr x pkgs; in {
            inherit julia;
            name = x;
            meta = julia.meta // {
              inherit language;
              baseName = "cpp11";
              displayName = "Julia " + julia.version;
              icon = ./logo-64x64.png;
            };
          }
    ) (filter (x: (hasAttr x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates);

  packageOptions = base@{julia, ...}: [];

  languageServerOptions = base@{julia, ...}: packages: {

  };
}
