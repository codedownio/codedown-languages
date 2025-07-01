{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "octave";

  kernels.octave.enable = true;
  kernels.octave.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';
}
