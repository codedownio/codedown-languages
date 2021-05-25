{ lib
, callPackage
, runCommand
, writeText
, makeWrapper
, python3
, bashInteractive
, octave
, extraJupyterConfig
}:

let
  common = callPackage ../common.nix {};

  fetchPypi = python3.pkgs.fetchPypi;
  buildPythonPackage = python3.pkgs.buildPythonPackage;

  metakernel = buildPythonPackage rec {
    pname = "metakernel";
    version = "0.27.5";

    src = fetchPypi {
      inherit pname version;
      sha256 = "0aqq9zil6h7kxsg3v2008nr6lv47qvcsash8qzmi1xh6r4x606zy";
    };

    buildInputs = with python3.pkgs; [ipykernel];

    # Tests hang, so disable
    doCheck = false;

    meta = {
      description = "Metakernel for Jupyter";
      homepage = https://github.com/Calysto/metakernel;
      license = lib.licenses.bsd3;
      maintainers = with lib.maintainers; [ thomasjm ];
    };
  };

  octaveKernel = buildPythonPackage rec {
    pname = "octave_kernel";
    version = "0.32.0";

    src = fetchPypi {
      inherit pname version;
      sha256 = "0dfbxfcf3bz4jswnpkibnjwlkgy0y4j563nrhaqxv3nfa65bksif";
    };

    buildInputs = with python3.pkgs; [ metakernel ipykernel ];

    # Tests failing because jupyter_kernel_test not available
    doCheck = false;

    meta = {
      description = "A Jupyter kernel for Octave.";
      homepage = https://github.com/Calysto/octave_kernel;
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

  python = runCommand "python" {
    inherit octave;
    python = python3.withPackages (ps: [metakernel octaveKernel] ++ (with ps; [traitlets jupyter_core ipykernel]));
    bash = bashInteractive;
    buildInputs = [makeWrapper];
  } ''
    # Important: make sure octave is on the PATH so that the kernel can find it.
    # Also, make sure a reasonable bash (not the non-interactive one) is available or else the shell magic
    # startup will hang.
    mkdir -p $out/bin/
    makeWrapper $python/bin/python $out/bin/python ${extraJupyterConfigArgs} \
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
    logo32 = ./logo-32x32.png;
    logo64 = ./logo-64x64.png;
    metadata = {
      codedown = {
        priority = 1;
      };
    };
  };
}
