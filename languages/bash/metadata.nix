{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  language = "bash";

  baseOptions = let
    baseCandidates = [
      "bashInteractive"
      "bashInteractive_5"
    ];
  in
    map (x:
      let bash = getAttr x pkgs; in {
            inherit bash;
            name = x;
            displayName = "Bash " + bash.version;
            meta = bash.meta;
            logo = ./bash.png;
          }
    ) (filter (x: hasAttr x pkgs) baseCandidates);

  baseByName = name: lib.findSingle (x: x.name == name) null "multiple" baseOptions;

  packageOptions = base@{...}: {};

  packageSearch = base@{...}: common.searcher {};

  languageServerOptions = base@{python, ...}: packages: {};
}
