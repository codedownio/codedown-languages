{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.octave.enable = true;
  kernels.octave.settings.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';
}
