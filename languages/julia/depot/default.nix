{julia, python, jupyter, runCommand, makeWrapper, stdenv, callPackage}:

let
  gr = import ./gr_binary.nix;

  extraLibs = [];

  pythonWithPackages = python.withPackages (ps: [ps.matplotlib]);

  # Wrapped Julia with libraries and environment variables.
  # Note: setting The PYTHON environment variable is recommended to prevent packages
  # from trying to obtain their own with Conda.
  juliaWithPackages = runCommand "julia-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${julia}/bin/julia $out/bin/julia \
                --suffix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath extraLibs}" \
                --set PYTHON ${pythonWithPackages}/bin/python \
                --set JUPYTER ${jupyter}/bin/jupyter \
                --set GRDIR ${gr}
  '';

in

(callPackage ./common.nix {
  julia = juliaWithPackages;
  makeWrapperArgs = "--prefix JULIA_DEPOT_PATH : /home/user/.julia";
}) // { python = pythonWithPackages; }
