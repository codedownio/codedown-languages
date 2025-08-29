{ callPackage
, runCommand
, writeText
, makeWrapper
, bashInteractive
, octave
, octave-kernel
, extraJupyterConfig

, attrs
, extensions
, version
}:

let
  common = callPackage ../common.nix {};

  extraJupyterConfigArgs = if extraJupyterConfig == null then "" else
    let
      octaveKernelConfig = writeText "octave_kernel_config.py" extraJupyterConfig;

      jupyterConfigFolder = runCommand "octave-jupyter-config" {} ''
        mkdir -p $out
        cp ${octaveKernelConfig} $out/octave_kernel_config.py
      '';
    in
      ''--suffix JUPYTER_CONFIG_PATH ":" ${jupyterConfigFolder}'';

  finalLauncher = runCommand "python" {
    inherit octave;
    bash = bashInteractive;
    buildInputs = [makeWrapper];
  } ''
    # Make sure a reasonable bash (not the non-interactive one) is available or else the shell magic
    # startup will hang.
    mkdir -p $out/bin/
    makeWrapper ${octave-kernel.launcher}/bin/octave-kernel $out/bin/octave-kernel ${extraJupyterConfigArgs} \
      --prefix PATH ":" $bash/bin
  '';

in

common.makeJupyterKernel {
  octave = {
    displayName = "Octave";
    argv = [
      "${finalLauncher}/bin/octave-kernel"
      "-f"
      "{connection_file}"
    ];
    language = "octave";
    logo32 = ./octave-logo-32x32.png;
    logo64 = ./octave-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = version;

        priority = 1;
      };
    };
  };
}
