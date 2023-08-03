{ lib
, callPackage
, fetchurl
, llvmPackages_9
, python3Packages

, blas

, attrName
, attrs
, displayName
, extensions
, metaOnly
, std
}:

with lib;

let
  common = callPackage ../common.nix {};

  cling = callPackage ./cling {};

  xeus-cling = callPackage ./xeus-cling/xeus-cling.nix { inherit cling; };

  xeusMisc = callPackage ./xeusMisc.nix;

in

common.makeJupyterKernel (
  listToAttrs [{
    name = attrName;
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
      language = attrName;
      logo32 = fetchurl {
        url = https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/ISO_C%2B%2B_Logo.svg/32px-ISO_C%2B%2B_Logo.svg.png;
        hash = "sha256-cr0TB8/j2mkcFhfCkz9F7ZANOuTlWA2OcWtDcXyOjHw=";
      };
      logo64 = fetchurl {
        url = https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/ISO_C%2B%2B_Logo.svg/64px-ISO_C%2B%2B_Logo.svg.png;
        hash = "sha256-nZtJ4bR7GmQttvqEJC9KejOxphrjjxT36L9yOIITFLk=";
      };
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
      env = {

      };
    };
  }]
)
