{ lib
, callPackage

, cling
, xeus-cling

, kernelName
, attrs
, displayName
, extensions
, std
}:

with lib;

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel (
  listToAttrs [{
    name = kernelName;
    value = {
      displayName = displayName;
      argv = [
        "${xeus-cling}/bin/xcpp"
      ]
      ++ cling.flags
      ++ [
        "-resource-dir" "${cling.unwrapped}"
        "-L" "${cling.unwrapped}/lib"
        "-l" "${cling.unwrapped}/lib/cling.so"
        "-std=${std}"
        # "-v"
        "-f" "{connection_file}"
      ];
      language = kernelName;
      logo32 = "${xeus-cling}/share/jupyter/kernels/xcpp17/logo-32x32.png";
      logo64 = "${xeus-cling}/share/jupyter/kernels/xcpp17/logo-64x64.png";
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = std;
          priority = 1;
        };
      };
      env = {

      };
    };
  }]
)
