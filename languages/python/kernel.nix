{ stdenv
, pkgs
, callPackage
, python
, displayName
, enableVariableInspector
, attrs
, extensions
, metaOnly ? false
}:

with pkgs.lib;

let
  common = callPackage ../common.nix {};

  variableInspector = {
    initial_code_path = ./variable_inspector.py;
    list_variables_command = "_codedown_variableinspector_dict_list()";
    inspect_variable_command = "print('TODO')";
  };

  repls = [{
    display_name = "IPython " + python.pkgs.ipython.version;
    proc = "${python.pkgs.ipython}/bin/ipython";
  }];

in

common.makeJupyterKernelInner metaOnly (
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
          inherit attrs extensions repls;
          variable_inspector = if enableVariableInspector then variableInspector else null;
          priority = 1;
        };
      };
    };
  }]
)
