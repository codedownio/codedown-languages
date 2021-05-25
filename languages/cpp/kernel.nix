{ lib, callPackage, blas, cling }:

with lib;

let
  common = callPackage ../common.nix {};

  # cling = callPackage ./cling.nix {};
  xeusStuff = callPackage ./xeusCling.nix { cling = cling.unwrapped; };
  xeusMisc = callPackage ./xeusMisc.nix {xtl = xeusStuff.xtl;};

  # clingKernel = python38Packages.buildPythonApplication {
  #   pname = "jupyter-cling-kernel";
  #   version = "0.7";
  #   src = "${cling}/share/cling/Jupyter/kernel";
  #   propagatedBuildInputs = with python38Packages; [ipykernel traitlets cling];
  # };

in

displayName: std: attrName: logo64: common.makeJupyterKernel (
  listToAttrs [{
    name = attrName;
    value = {
      displayName = displayName;
      argv =
        # ["${clingKernel}/bin/jupyter-cling-kernel"]
        ["${xeusStuff.xeusCling}/bin/xcpp"]
        ++ cling.flags
        ++ [
          "-resource-dir" "${cling}"

          # Be able to use libraries installed by Nix
          "-I" "/home/user/.nix-profile/include"
          "-L" "/home/user/.nix-profile/lib"

          # xtensor and xtensor-blas (used in sample notebook)
          "-idirafter" "${xeusMisc.xtensor}/include"
          "-idirafter" "${xeusMisc.xtensorBlas}/include"
          "-L" "${xeusMisc.liblapackShared}/lib"
          "-L" "${blas}/lib"
        ]
        ++ [
          "-f" "{connection_file}"
          "--std=${std}"
        ];
      language = attrName;
      logo32 = null;
      logo64 = logo64;
      metadata = {
        codedown = {
          priority = 1;
        };
      };
    };
  }]
)
