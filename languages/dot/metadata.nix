{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseCandidates = [
    "graphviz"
  ];
  baseOptions = map (x:
    let graphviz = getAttr x pkgs; in {
      inherit graphviz;
      name = x;
      displayName = "Graphviz " + graphviz.version;
      meta = graphviz.meta;
      logo = ./logo-64x64.png;
    }
  ) (filter (x: hasAttr x pkgs) baseCandidates);

  packageOptions = base@{...}: {};

  languageServerOptions = base@{}: packages: {};
}
