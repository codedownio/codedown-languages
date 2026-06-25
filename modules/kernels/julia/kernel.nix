{ lib
, callPackage
, python
, julia
, displayName
, writeShellScript

, enableVariableInspector

, attrs
, extensions
}:

with lib;

let
  common = callPackage ../common.nix {};

  variableInspector = {
    initial_code_path = ./variable_inspector.jl;
    list_variables_command = "CodedownVariableInspector.dict_list()";
    inspect_variable_command = "CodedownVariableInspector.inspect(\"{{VARIABLE_NAME}}\")";
  };

  runJuliaKernel = writeShellScript "run-julia-kernel.sh" ''
    ${julia}/bin/julia --startup-file=yes --color=yes -e "
      import IJulia
      using Pkg: pkgversion

      kernel_path = joinpath(pathof(IJulia) |> dirname, \"kernel.jl\")
      include(kernel_path)

      if pkgversion(IJulia) >= v\"1.27.0\"
        run_kernel()
      end
    " $1
  '';

in

common.makeJupyterKernel (
  listToAttrs [{
    name = head attrs;
    value = {
      inherit displayName;
      argv = [
        "${runJuliaKernel}"
        "{connection_file}"
      ];
      language = lib.head attrs;
      logo32 = ./julia-logo-32x32.png;
      logo64 = ./julia-logo-64x64.png;
      env = {
        LC_ALL = "C";
        PYTHON = ''${python}/bin/python'';
        PYTHONPATH = ''${python}/${python.sitePackages}'';
      };
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = julia.version;
          variable_inspector = if enableVariableInspector then variableInspector else null;
          priority = 1;
        };
      };
    };
  }]
)
