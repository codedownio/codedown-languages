{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  language = "octave";

  packageOptions = base@{octave, ...}: octave.pkgs;

  languageServerOptions = base@{octave, ...}: {};

  defaultJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';
}
