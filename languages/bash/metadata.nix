{ pkgs }:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../common.nix {};

in

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
            meta = bash.meta // {
              inherit language;
              baseName = x;
              displayName = "Bash " + bash.version;
              icon = ./bash.png;
            };
          }
    ) (filter (x: hasAttr x pkgs) baseCandidates);

  baseByName = name: lib.findSingle (x: x.name == name) null "multiple" baseOptions;

  packageOptions = base@{...}: {};

  packageSearch = base@{...}: common.searcher {};
}
