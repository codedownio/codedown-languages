{ lib
, callPackage
, blas
, python3Packages

, attrs
, extensions
, logo64
, std

, metaOnly
}:

with lib;

let
  common = callPackage ../common.nix {};

  cling = callPackage ./cling {};
  xeusStuff = callPackage ./xeus/xeusCling.nix { cling = cling.unwrapped; };
  xeusMisc = callPackage ./xeus/xeusMisc.nix {xtl = xeusStuff.xtl;};

  # clingKernel = python3Packages.buildPythonApplication {
  #   pname = "jupyter-cling-kernel";
  #   version = "0.9";
  #   src = "${cling}/share/cling/Jupyter/kernel";
  #   propagatedBuildInputs = with python3Packages; [ipykernel traitlets cling];
  # };

in

displayName: attrName: common.makeJupyterKernel (
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
          # "-I" "/home/user/.nix-profile/include"
          # "-L" "/home/user/.nix-profile/lib"

          # xtensor and xtensor-blas (used in sample notebook)
          # "-idirafter" "${xeusMisc.xtensor}/include"
          # "-idirafter" "${xeusMisc.xtensorBlas}/include"
          # "-L" "${xeusMisc.liblapackShared}/lib"
          # "-L" "${blas}/lib"
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
          inherit attrs extensions;
          priority = 1;
        };
      };
      env = {
        # "JUPYTER_CLING_KERNEL" = "${clingKernel}";
      };
    };
  }]
)
