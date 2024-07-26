{ callPackage

, mkCodeDownEnvironment
, name ? "codedown-environment"
}:

{
  channels ? {}
  , packages ? {}
}:

mkCodeDownEnvironment {
  channels = null;
  environmentName = name;
  kernels = [];
  otherPackages = [];
}
