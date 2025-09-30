{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "octave";

  kernels.octave.enable = true;
  kernels.octave.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';

  # kernels.octave.packages = ["doctest"]; # TODO: uncomment after https://github.com/NixOS/nixpkgs/pull/447122
}
