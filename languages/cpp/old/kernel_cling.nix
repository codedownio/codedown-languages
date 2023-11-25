{ lib
, blas
, callPackage
, gnused
, makeWrapper
, python3
, runCommand

, attrs
, extensions
, logo64
, std

, metaOnly
}:

with lib;

let
  cling = callPackage ./cling {};

  common = callPackage ../common.nix {};

  python = python3.withPackages (ps: with ps; [ipykernel traitlets]);

  pythonWrapped = runCommand "python-cling-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${python}/bin/python $out/bin/python \
      --prefix PATH ":" ${cling.unwrapped}/bin \
      --prefix PATH ":" ${gnused}/bin
  '';

  # clingKernel = python3Packages.buildPythonApplication {
  #   pname = "jupyter-cling-kernel";
  #   version = "0.9";
  #   src = "${cling}/share/cling/Jupyter/kernel";
  #   propagatedBuildInputs = with python3Packages; [ipykernel traitlets cling];
  # };


  # "JUPYTER_CLING_KERNEL" = "${clingKernel}";

in

displayName: attrName: common.makeJupyterKernel (
  listToAttrs [{
    name = attrName;
    value = {
      inherit displayName;
      argv =
        [
          "${pythonWrapped}/bin/python"
          "${cling.unwrapped}/share/Jupyter/kernel/clingkernel.py"
          "{connection_file}"
        ]
        # ++ cling.flags
        ++ [
          # Be able to use libraries installed by Nix
          # "-I" "/home/user/.nix-profile/include"
          # "-L" "/home/user/.nix-profile/lib"

          # xtensor and xtensor-blas (used in sample notebook)
          # "-idirafter" "${xeusMisc.xtensor}/include"
          # "-idirafter" "${xeusMisc.xtensorBlas}/include"
          # "-L" "${xeusMisc.liblapackShared}/lib"
          # "-L" "${blas}/lib"
        ]
        # ++ [
        #   "-f" "{connection_file}"
        #   "--std=${std}"
        # ]
      ;
      language = attrName;
      logo32 = null;
      logo64 = logo64;
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
      env = {};
    };
  }]
)
