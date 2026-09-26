{ lib
, callPackage

, tslab
, nodeModules

, attrs
, extensions
, version
, repls ? {}

, kernelName
, displayName
, isTypescript
}:

with lib;

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel (
  listToAttrs [{
    name = head attrs;
    value = {
      inherit displayName;
      language = head attrs;
      argv = [
        "${tslab}/bin/tslab"
        "kernel"
        "--config-path"
        "{connection_file}"
      ]
      ++ optional (!isTypescript) "--js";
      logo32 = if isTypescript then ../typescript/typescript-logo-32x32.png else ./javascript-logo-32x32.png;
      logo64 = if isTypescript then ../typescript/typescript-logo-64x64.png else ./javascript-logo-64x64.png;

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

          repls = common.replsToMetadata kernelName repls;

          priority = 1;
        };
      };
    };
  }]
)
