{ callPackage
, lib

, coq
, coq-kernel

, displayName
# , enableVariableInspector

, isRocq
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

  coqKernelToUse = coq-kernel.override {
    inherit coq;
  };

in

common.makeJupyterKernel (
  lib.listToAttrs [{
    name = lib.head attrs;
    value = {
      displayName = displayName;
      language = lib.head attrs;
      argv = [
        "${coqKernelToUse}/bin/coq-kernel"
        "-f"
        "{connection_file}"
      ];
      logo32 = "${coqKernelToUse.logos}/logo-32x32.png";
      logo64 = "${coqKernelToUse.logos}/logo-64x64.png";
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = coq.version;

          # variable_inspector = if enableVariableInspector then variableInspector else null;

          priority = 1;
        };
      };
      env = lib.listToAttrs [
        {
          name = if isRocq then "ROCQPATH" else "COQPATH";
          value = lib.concatStringsSep ":" (
            map (x: "${x}/lib/coq/${coq.coq-version}/user-contrib/") chosenPackages
          );
        }
        {
          name = "OCAMLPATH";
          value = lib.concatStringsSep ":" (
            map (x: "${x}/lib/ocaml/${coq.ocaml.version}/site-lib/") ([ coq.ocamlPackages.findlib ] ++ chosenPackages)
          );
        }
      ];
    };
  }]
)
