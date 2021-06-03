{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseCandidates = [
    "ruby"
    "ruby_2_6"
    "ruby_2_7"
    "ruby_3_0"
  ];
  baseOptions = map (x:
    let ruby = getAttr x pkgs; in {
      inherit ruby;
      name = x;
      displayName = "Ruby " + julia.version;
      meta = ruby.meta;
      logo = ./logo-64x64.png;
    }
  ) (filter (x: (hasAttr x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates);

  packageOptions = base@{ruby, ...}: [];

  packageSearch = base@{ruby, ...}: common.searcher ruby.gems;

  languageServerOptions = base@{ruby, ...}: packages: {

  };
}
