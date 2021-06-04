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
      meta = graphviz.meta // {
        displayName = "Graphviz " + graphviz.version;
        icon = ./logo-64x64.png;
      };
    }
  ) (filter (x: hasAttr x pkgs) baseCandidates);

  packageOptions = base@{...}: {};

  languageServerOptions = base@{}: packages: {};
}
