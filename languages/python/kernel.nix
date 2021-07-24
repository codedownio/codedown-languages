{ stdenv
, pkgs
, callPackage
, python
, displayName
, enableVariableInspector
, attrs
, extensions
}:

with pkgs.lib;

let
  common = callPackage ../common.nix {};

  variableInspector = {
    initial_code_path = ./variable_inspector.py;
    list_variables_command = "_codedown_variableinspector_dict_list()";
    inspect_variable_command = "print('TODO')";
  };

in

common.makeJupyterKernel (
  listToAttrs [{
    name = head attrs;
    value = {
      displayName = displayName;
      language = head attrs;
      argv = [
        "${python}/bin/python"
        "-m"
        "ipykernel"
        "-f"
        "{connection_file}"
      ];
      logo32 = ./logo-32x32.png;
      logo64 = ./logo-64x64.png;
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
