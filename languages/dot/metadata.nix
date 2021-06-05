{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  language = "dot";

  baseOptions = let
    baseCandidates = [
      "graphviz"
    ];
  in
    map (x:
      let graphviz = getAttr x pkgs; in {
            inherit graphviz;
            name = x;
            meta = graphviz.meta // {
              inherit language;
              baseName = "cpp11";
              displayName = "Graphviz " + graphviz.version;
              icon = ./logo-64x64.png;
            };
          }
    ) (filter (x: hasAttr x pkgs) baseCandidates);

  packageOptions = base@{...}: {};

  languageServerOptions = base@{}: packages: {};
}
