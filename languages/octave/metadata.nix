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
      meta = octave.meta // {
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
