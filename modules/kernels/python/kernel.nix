{ lib
, callPackage
, python

, kernelName
, displayName

, enableVariableInspector

, attrs
, extensions
}:

with lib;

let
  common = callPackage ../common.nix {};

  variableInspector = {
    initial_code_path = ./variable_inspector.py;
    list_variables_command = "_codedown_variable_inspector.list()";
    inspect_variable_command = "_codedown_variable_inspector.inspect('{{VARIABLE_NAME}}')";
  };

in

common.makeJupyterKernel (
  listToAttrs [{
    name = kernelName;
    value = {
      displayName = displayName;
      language = kernelName;
      argv = [
        "${python}/bin/python"
        "-m"
        "ipykernel"
        "-f"
        "{connection_file}"
      ];
      logo32 = ./python-logo-32x32.png;
      logo64 = ./python-logo-64x64.png;
      env = { COLUMNS = "80"; };
      metadata = {
        codedown = {
          inherit attrs extensions;
          variable_inspector = if enableVariableInspector then variableInspector else null;
          priority = 1;
        };
      };
    };
  }]
)
