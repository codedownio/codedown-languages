{ codedown
, channels ? {}
, ...
}:

codedown.mkCodeDownEnvironment {
  environmentName = "shells-bash";
  inherit channels;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.bash"; contents = codedown.shells.bash; }
  ];
}

# codedown.makeEnvironmentPrime {
#   inherit channels;

#   packages = {
#     "codedown.shells.bash" = {
#       packages = {};
#     };
#   };
# }
