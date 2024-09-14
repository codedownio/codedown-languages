{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.octave.enable = true;
  kernels.octave.settings.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';
}
