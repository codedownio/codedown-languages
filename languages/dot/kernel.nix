{ python3
, callPackage
, graphviz
, fetchFromGitHub
, makeFontsConf
, fontconfig
, carlito
, dejavu_fonts
, freefont_ttf
, xorg
, attrs
, extensions
}:

let
  common = callPackage ../common.nix {};

  dotKernel = python3.pkgs.buildPythonPackage rec {
    name = "dot_kernel";

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "jupyter-dot-kernel";
      rev = "2aeac0bf05824fa36307b58caa298c4f3c31789d";
      sha256 = "1gmvybwfj6m1sjk1h6350446z3wfnai2062sfs0zgs3lczhflxsc";
    };

    buildInputs = with python3.pkgs; [ jupyter_client ipykernel ];

    propagatedBuildInputs = with python3.pkgs; [ graphviz fontconfig ipykernel ];

    patchPhase = ''
      sed -i 's|, "jupyter"||g' setup.py
    '';

    doCheck = false;

    meta = {
      description = "Dot language kernel for Jupyter";
      homepage = https://github.com/laixintao/jupyter-dot-kernel;
    };
  };

  fontsConf = makeFontsConf {
    fontDirectories = [
      carlito dejavu_fonts
      freefont_ttf xorg.fontmiscmisc
      # liberation_ttf_v1_binary
      # liberation_ttf_v2_binary
    ];
  };

  pythonWithPackages = python3.withPackages (ps: [dotKernel]);

in

common.makeJupyterKernel {
  dot = {
    displayName = "Dot (Graphviz)";
    argv = [
      "${pythonWithPackages}/bin/python"
      "-m"
      "dot_kernel"
      "-f"
      "{connection_file}"
    ];
    language = "dot";
    logo32 = ./dot-logo-32x32.png;
    logo64 = ./dot-logo-64x64.png;
    env = { FONTCONFIG_FILE = "${fontsConf}"; };
    metadata = {
      codedown = {
        inherit attrs extensions;
        priority = 10;
      };
    };
  };
}
