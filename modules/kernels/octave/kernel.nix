{ callPackage
, bashInteractive
, lib
, makeWrapper
, octave
, python3
, runCommand
, writeText

, extraJupyterConfig

, attrs
, extensions
, version
}:

let
  common = callPackage ../common.nix {};

  inherit (python3.pkgs) buildPythonPackage fetchPypi;

  octaveKernel = buildPythonPackage rec {
    pname = "octave_kernel";
    version = "0.36.0";

    format = "pyproject";

    src = fetchPypi {
      inherit pname version;
      sha256 = "sha256-kliC+JFchu11ttuOE0He7ZgNugvnO/BzZdPqCJYaHds=";
    };

    nativeBuildInputs = with python3.pkgs; [ hatchling ];

    propagatedBuildInputs = with python3.pkgs; [ metakernel ipykernel ];

    # Tests failing because jupyter_kernel_test not available
    doCheck = false;

    meta = {
      description = "A Jupyter kernel for Octave.";
      homepage = "https://github.com/Calysto/octave_kernel";
      license = lib.licenses.bsd3;
      maintainers = with lib.maintainers; [ thomasjm ];
    };
  };

  extraJupyterConfigArgs = if extraJupyterConfig == null then "" else
    let
      octaveKernelConfig = writeText "octave_kernel_config.py" extraJupyterConfig;

      jupyterConfigFolder = runCommand "octave-jupyter-config" {} ''
        mkdir -p $out
        cp ${octaveKernelConfig} $out/octave_kernel_config.py
      '';
    in
      ''--suffix JUPYTER_CONFIG_PATH ":" ${jupyterConfigFolder}'';

  extraOctaveConfig = ''
    warning('off', 'Octave:gnuplot-graphics')
  '';

  extraOctaveConfigArgs = if extraOctaveConfig == null then "" else
    let
      octaverc = writeText "octaverc" extraOctaveConfig;
    in
      ''--set OCTAVE_SITE_INITFILE ${octaverc}'';

  python = runCommand "python" {
    inherit octave;
    python = python3.withPackages (ps: [ps.metakernel octaveKernel] ++ (with ps; [traitlets jupyter_core ipykernel]));
    bash = bashInteractive;
    buildInputs = [makeWrapper];
  } ''
    # Important: make sure octave is on the PATH so that the kernel can find it.
    # Also, make sure a reasonable bash (not the non-interactive one) is available or else the shell magic
    # startup will hang.
    mkdir -p $out/bin/
    makeWrapper $python/bin/python $out/bin/python ${extraJupyterConfigArgs} ${extraOctaveConfigArgs} \
      --prefix PATH ":" $bash/bin \
      --suffix PATH ":" $octave/bin
  '';

in

common.makeJupyterKernel {
  octave = {
    displayName = "Octave";
    argv = [
      "${python}/bin/python"
      "-m"
      "octave_kernel"
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
