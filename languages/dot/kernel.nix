with import <nixpkgs> {};
with python3Packages;

let
  dotKernel = buildPythonPackage rec {
    name = "dot_kernel";

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "jupyter-dot-kernel";
      rev = "2aeac0bf05824fa36307b58caa298c4f3c31789d";
      sha256 = "1gmvybwfj6m1sjk1h6350446z3wfnai2062sfs0zgs3lczhflxsc";
    };

    buildInputs = [ jupyter_client ipykernel ];

    propagatedBuildInputs = [ graphviz fontconfig ipykernel ];

    patchPhase = ''
      sed -i 's|, "jupyter"||g' setup.py
    '';

    doCheck = false;

    meta = {
      description = "Dot language kernel for Jupyter";
      homepage = https://github.com/laixintao/jupyter-dot-kernel;
    };
  };

in

python.withPackages (ps: [dotKernel])
