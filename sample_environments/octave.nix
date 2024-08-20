{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.octave" = {
      packages = {};
      extraJupyterConfig = ''
        c.OctaveKernel.plot_settings = dict(format='svg')
      '';
    };
  };
}
