{ lib
, callPackage

, xeus-cpp

, kernelName
, attrs
, displayName
, extensions
, std
, repls ? {}
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
        "${xeus-cpp}/bin/xcpp"
        "-std=${std}"
        "-f" "{connection_file}"
      ];
      language = kernelName;
      logo32 = "${xeus-cpp}/share/jupyter/kernels/xcpp17/logo-32x32.png";
      logo64 = "${xeus-cpp}/share/jupyter/kernels/xcpp17/logo-64x64.png";
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = std;
          repls = common.replsToMetadata kernelName repls;
          priority = 1;
        };
      };
      env = {

      };
    };
  }]
)
