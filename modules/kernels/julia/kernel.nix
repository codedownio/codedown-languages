{ lib
, callPackage
, python
, julia
, displayName
, gnugrep
, writeShellScript

, attrs
, extensions
}:

with lib;

let
  common = callPackage ../common.nix {};

  runJuliaKernel = writeShellScript "run-julia-kernel.sh" ''
    export PATH="${lib.makeBinPath [gnugrep]}:$PATH"
    kernelFilePath=$(find ${julia.projectAndDepot}/depot/packages/IJulia -name kernel.jl)
    # Check if IJulia version has run_kernel() function (v1.27+)
    if grep -q "function run_kernel()" "$kernelFilePath"; then
      # For newer IJulia versions, include and call run_kernel() function
      ${julia}/bin/julia --startup-file=yes --color=yes -e "using IJulia; include(\"$kernelFilePath\"); run_kernel()" $1
    else
      # For older IJulia versions, use interactive mode
      ${julia}/bin/julia -i --startup-file=yes --color=yes $kernelFilePath $1
    fi
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
          priority = 1;
        };
      };
    };
  }]
)
