{ lib
, callPackage

, iruby

, enableVariableInspector

, attrs
, extensions
, version
}:

with lib;

let
  common = callPackage ../common.nix {};

  variableInspector = {
    initial_code_path = ./variable_inspector.rb;
    list_variables_command = "CodedownVariableInspector.list(binding)";
    inspect_variable_command = "CodedownVariableInspector.inspect_var(binding, \"{{VARIABLE_NAME}}\")";
  };

in

common.makeJupyterKernel (
  listToAttrs [{
    name = head attrs;
    value = {
      displayName = "Ruby";
      language = head attrs;
      argv = [
        "${iruby}/bin/iruby"
        "kernel"
        "{connection_file}"
      ];
      logo32 = ./iruby-32x32.png;
      logo64 = ./iruby-64x64.png;
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = version;

          variable_inspector = if enableVariableInspector then variableInspector else null;

          priority = 1;
        };
      };
    };
  }]
)
