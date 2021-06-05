{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  language = "octave";

  baseOptions = let
    baseCandidates = [
      "octave"
    ];
  in
    map (x:
      let octave = getAttr x pkgs; in {
            inherit octave;
            name = x;
            meta = octave.meta // {
              inherit language;
              baseName = x;
              displayName = "Octave " + octave.version;
              icon = ./logo-64x64.png;
            };
          }
    ) (filter (x: hasAttr x pkgs) baseCandidates);

  packageOptions = base@{octave, ...}: octave.pkgs;

  languageServerOptions = base@{octave, ...}: {};

  defaultJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';
}
