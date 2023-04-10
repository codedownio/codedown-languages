{ lib
, callPackage
, python
, julia
, displayName
, writeShellScript

, attrs
, extensions
, metaOnly ? false
}:

with lib;

let
  common = callPackage ../common.nix {};

  runJuliaKernel = writeShellScript "run-julia-kernel.sh" ''
    kernelFilePath=$(find ${julia.projectAndDepot}/depot/packages/IJulia -name kernel.jl)
    ${julia}/bin/julia -i --startup-file=yes --color=yes $kernelFilePath $1
  '';

in

common.makeJupyterKernelInner metaOnly (
  listToAttrs [{
    name = head attrs;
    value = {
      inherit displayName;
      argv = [
        "${runJuliaKernel}"
        "{connection_file}"
      ];
      language = lib.head attrs;
      logo32 = ./logo-32x32.png;
      logo64 = ./logo-64x64.png;
      env = {
        LC_ALL = "C";
        PYTHON = ''${python}/bin/python'';
        PYTHONPATH = ''${python}/${python.sitePackages}'';
      };
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
    };
  }]
)
