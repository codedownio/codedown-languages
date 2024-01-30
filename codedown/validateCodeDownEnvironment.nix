{ lib
}:

args@{
  channels
  , kernels ? []
  , otherPackages ? []
  , ...
}:

with lib;

let
  shellsCommon = callPackage ../shells/common.nix {};

  validateKernel = kernel: {

  };

  validateOtherPackage = kernel: {

  };

in

{
  channels = {};
  kernels = map validateKernel kernels;
  otherPackages = map validateOtherPackage otherPackages;
}
