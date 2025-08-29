{ callPackage
, lib

, coq
, coq_jupyter

, displayName
, enableVariableInspector

, chosenPackages
, attrs
, extensions
}:

let
  common = callPackage ../common.nix {};

  # variableInspector = {
  #   initial_code_path = ./variable_inspector.py;
  #   list_variables_command = "_codedown_variableinspector_dict_list()";
  #   inspect_variable_command = "print('TODO')";
  # };

in

common.makeJupyterKernel (
  lib.listToAttrs [{
    name = lib.head attrs;
    value = {
      displayName = displayName;
      language = lib.head attrs;
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
          language_version = coq.version;

          # variable_inspector = if enableVariableInspector then variableInspector else null;

          priority = 1;
        };
      };
      env = {
        COQPATH = lib.concatStringsSep ":" (map (x: "${x}/lib/coq/${coq.coq-version}/user-contrib/") chosenPackages);
      };
    };
  }]
)
