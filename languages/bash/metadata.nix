{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  language = "bash";

  baseCandidates = [
    "bashInteractive"
    "bashInteractive_5"
  ];
  baseOptions = map (x:
    let bash = getAttr x pkgs; in {
      inherit bash;
      name = x;
      displayName = "bash " + python.version;
      meta = bash.meta;
      logo = ./bash.png;
    }
  ) (filter (x: hasAttr x pkgs) baseCandidates);

  packageOptions = base@{python, ...}: {};

  languageServerOptions = base@{python, ...}: packages: {};
}
