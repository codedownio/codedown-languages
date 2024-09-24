{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.octave.enable = true;
  kernels.octave.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';
}
