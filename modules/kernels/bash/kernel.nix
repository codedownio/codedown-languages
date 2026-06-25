{ lib
, callPackage
, python3
, bashInteractive

, enableVariableInspector

, attrs
, extensions
}:

let
  common = callPackage ../common.nix {};

  python = python3.withPackages (ps: [ps.bash-kernel]);

  # Checks failed on macOS on release-25.05. Disabling them is one option:
  # python = python3.withPackages (ps: [(ps.bash-kernel.overrideAttrs (_oldAttrs: { doCheck = false; }))]);

  variableInspector = {
    initial_code_path = ./variable_inspector.sh;
    list_variables_command = "__codedown_variable_inspector_list";
    inspect_variable_command = "__codedown_variable_inspector_inspect '{{VARIABLE_NAME}}'";
  };

in

common.makeJupyterKernel {
  bash = {
    displayName = "Bash";
    argv = [
      "${python}/bin/python"
      "-m"
      "bash_kernel"
      "-f"
      "{connection_file}"
    ];
    language = lib.head attrs;
    logo32 = ./bash-logo-128x128.png;
    logo64 = ./bash-logo-128x128.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = bashInteractive.version;
        variable_inspector = if enableVariableInspector then variableInspector else null;
        priority = 10;
      };
    };
  };
}
