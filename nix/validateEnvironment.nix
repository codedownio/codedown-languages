{ lib
}:

{ kernels ? []
  , otherPackages ? []
  , ...
}:

with lib;

let

  validateKernel = _kernel: {

  };

  validateOtherPackage = _kernel: {

  };

in

{
  channels = {};
  kernels = map validateKernel kernels;
  otherPackages = map validateOtherPackage otherPackages;
}
