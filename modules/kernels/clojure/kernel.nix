{ callPackage
, attrs
, extensions
, version
, clojupyter

, enableVariableInspector
}:

let
  common = callPackage ../common.nix {};

  variableInspector = {
    initial_code_path = ./variable_inspector.clj;
    list_variables_command = "(codedown-variable-inspector-list)";
    inspect_variable_command = "(codedown-variable-inspector-inspect \"{{VARIABLE_NAME}}\")";
  };

in

common.makeJupyterKernel {
  clojure = {
    displayName = "Clojure";
    argv = [
      "${clojupyter.launcher}/bin/clojupyter"
      "{connection_file}"
    ];
    language = "clojure";
    logo32 = ./clojure-logo-32x32.png;
    logo64 = ./clojure-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = version;
        variable_inspector = if enableVariableInspector then variableInspector else null;
        priority = 1;
      };
    };
  };
}
