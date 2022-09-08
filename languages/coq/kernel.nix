{ stdenv
, pkgs
, callPackage
, coq
, displayName
, enableVariableInspector
, attrs
, extensions
, metaOnly ? false
}:

with pkgs.lib;

let
  common = callPackage ../common.nix {};

  coq_jupyter = callPackage ./coq_jupyter {};

  # variableInspector = {
  #   initial_code_path = ./variable_inspector.py;
  #   list_variables_command = "_codedown_variableinspector_dict_list()";
  #   inspect_variable_command = "print('TODO')";
  # };

in

common.makeJupyterKernelInner metaOnly (
  listToAttrs [{
    name = head attrs;
    value = {
      displayName = displayName;
      language = head attrs;
      argv = [
        "${coq_jupyter.launcher}/bin/coq-kernel"
        "-f"
        "{connection_file}"
      ];
      logo32 = coq_jupyter.sizedLogo "32";
      logo64 = coq_jupyter.sizedLogo "64";
      metadata = {
        codedown = {
          inherit attrs extensions;

          # variable_inspector = if enableVariableInspector then variableInspector else null;
          variable_inspector = null;

          priority = 1;
        };
      };
    };
  }]
)