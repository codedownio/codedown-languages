{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "exporters-small";
  inherit channels;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "exporters.nbconvert-small"; contents = codedown.exporters.nbconvert-small; }
  ];
}

# codedown.makeEnvironment {
#   inherit channels;

#   packages = {
#     "codedown.exporters.nbconvert-small" = {};
#   };
# }
