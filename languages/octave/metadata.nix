{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseCandidates = [
    "octave"
  ];
  baseOptions = map (x:
    let octave = getAttr x pkgs; in {
      inherit octave;
      name = x;
      displayName = "Octave " + octave.version;
      meta = octave.meta;
      logo = ./logo-64x64.png;
    }
  ) (filter (x: hasAttr x pkgs) baseCandidates);

  packageOptions = base@{octave, ...}: {};

  languageServerOptions = base@{octave, ...}: {};
}
