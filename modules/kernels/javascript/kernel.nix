{ lib
, callPackage

, tslab
, nodeModules

, attrs
, extensions
, version
, repls ? {}
}:

with lib;

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel (
  listToAttrs [{
    name = head attrs;
    value = {
      displayName = "JavaScript";
      language = head attrs;
      argv = [
        "${tslab}/bin/tslab"
        "kernel"
        "--config-path"
        "{connection_file}"
        "--js"
      ];
      logo32 = ./javascript-logo-32x32.png;
      logo64 = ./javascript-logo-64x64.png;

      # The kernel's working directory is the user's notebook directory, so the environment's
      # packages have to come in through the environment rather than through cwd.
      env = {
        NODE_PATH = "${nodeModules}/node_modules";
      };

      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = version;

          # tslab keeps cell state inside its own executor with no listing command, so there's
          # nothing to drive an inspector with yet.
          variable_inspector = null;

          repls = common.replsToMetadata "javascript" repls;

          priority = 1;
        };
      };
    };
  }]
)
