{ callPackage
, rWithPackages

# , lib
# , coreutils
# , which

, enableVariableInspector

, attrs
, extensions
, version
}:

let
  common = callPackage ../common.nix {};

  variableInspector = {
    initial_code_path = ./variable_inspector.R;
    list_variables_command = ".codedown_variable_inspector$dict_list()";
    inspect_variable_command = ".codedown_variable_inspector$inspect('{{VARIABLE_NAME}}')";
  };

in

common.makeJupyterKernel {
  r = {
    displayName = "R";
    argv = [
      "${rWithPackages}/bin/R"
      "--slave"
      "-e"
      "IRkernel::main()"
      "--args"
      "{connection_file}"
    ];
    language = "r";

    logo32 = null;
    logo64 = ./r-logo-64x64.png;

    # The kernel calls "which uname"
    # https://github.com/codedownio/codedown-languages/issues/63
    # env = {
    #   PATH = lib.makeBinPath [coreutils which];
    # };

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
