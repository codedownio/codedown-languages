{ callPackage
, python
, julia
, writeShellScriptBin
}:

let
  common = callPackage ../common.nix {};

  runJuliaKernel = writeShellScriptBin "run-julia-kernel.sh" ''
    kernelFilePath=$(find ${julia.depot}/packages/IJulia -name kernel.jl)
    ${julia}/bin/julia -i --startup-file=yes --color=yes $kernelFilePath $1
  '';

in

common.makeJupyterKernel {
  julia = {
    displayName = "Julia 1.5";
    argv = [
      "${runJuliaKernel}/bin/run-julia-kernel.sh"
      "{connection_file}"
    ];
    language = "julia";
    logo32 = ./logo-32x32.png;
    logo64 = ./logo-64x64.png;
    env = {
      LC_ALL = "C";
      PYTHON = ''${python}/bin/python'';
      PYTHONPATH = ''${python}/${python.sitePackages}'';
    };
    metadata = {
      codedown = {
        priority = 1;
      };
    };
  };
}
