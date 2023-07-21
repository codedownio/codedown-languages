{ lib
, callPackage
, llvmPackages_9
, python3Packages

, blas
, cling

, attrs
, extensions
, logo64
, std

, metaOnly
}:

with lib;

let
  common = callPackage ../common.nix {};

  xeusStuff = callPackage ./xeus/xeusCling.nix { cling = cling.unwrapped; };
  xeusMisc = callPackage ./xeus/xeusMisc.nix {xtl = xeusStuff.xtl;};

in

displayName: attrName: common.makeJupyterKernel (
  listToAttrs [{
    name = attrName;
    value = {
      displayName = displayName;
      argv =
        ["${xeusStuff.xeusCling}/bin/xcpp"]
        ++ [
          "-I" "${lib.getDev llvmPackages_9.libcxx}/include/c++/v1"
        ]
        ++ cling.flags
        ++ [
          "-resource-dir" "${cling.unwrapped}"

          "-l" "${llvmPackages_9.libcxx}/lib/libc++.so"

          # Uncomment to see some info about Cling's search path setup
          # "-v"

          # Be able to use libraries installed by Nix
          # "-I" "/home/user/.nix-profile/include"
          # "-L" "/home/user/.nix-profile/lib"

          # xtensor and xtensor-blas (used in sample notebook)
          "-idirafter" "${xeusMisc.xtensor}/include"
          "-idirafter" "${xeusMisc.xtensorBlas}/include"
          "-L" "${xeusMisc.liblapackShared}/lib"
          "-L" "${blas}/lib"
        ]
        ++ [
          "-f" "{connection_file}"
          "-std=${std}"
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

      };
    };
  }]
)
