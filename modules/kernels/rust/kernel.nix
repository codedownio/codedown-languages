{ lib
, callPackage

, evcxr

, enableVariableInspector

, displayName
, attrs
, extensions
, version
, language ? lib.head attrs
}:

with lib;

let
  common = callPackage ../common.nix {};

  # evcxr has no in-code introspection, so the listing comes from a :vars_json
  # command we added to evcxr (see ~/tools/evcxr), which prints the bound variables
  # and their types as JSON to stdout — matching the uniform inspector contract.
  variableInspector = {
    initial_code_path = ./variable_inspector.rs;
    list_variables_command = ":vars_json";
    inspect_variable_command = null;
  };

in

common.makeJupyterKernel (
  listToAttrs [{
    name = head attrs;
    version = evcxr.version;
    value = {
      inherit displayName;
      argv = [
        "${evcxr}/bin/evcxr_jupyter"
        "--control_file"
        "{connection_file}"
      ];
      inherit language;
      logo32 = ./rust-logo-32x32.png;
      logo64 = ./rust-logo-64x64.png;
      kernelSvg = ./rust-logo.svg;
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = version;
          other_versions = {
            evcxr = evcxr.version;
          };
          variable_inspector = if enableVariableInspector then variableInspector else null;
          priority = 1;
        };
      };
    };
  }]
)
