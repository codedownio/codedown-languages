let nixpkgs = import (import ../../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;


let
  baseJulia = julia_15;

  gr = import ./gr_binary.nix;

  extraLibs = [];

  python = python3.withPackages (ps: [ps.matplotlib]);

  # Wrapped Julia with libraries and environment variables.
  # Note: setting The PYTHON environment variable is recommended to prevent packages
  # from trying to obtain their own with Conda.
  julia = runCommand "julia-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${baseJulia}/bin/julia $out/bin/julia \
                --suffix LD_LIBRARY_PATH : "${lib.makeLibraryPath extraLibs}" \
                --set PYTHON ${python}/bin/python \
                --set JUPYTER ${jupyter}/bin/jupyter \
                --set GRDIR ${gr}
  '';

in

(callPackage ./common.nix {
  inherit julia;
  makeWrapperArgs = "--prefix JULIA_DEPOT_PATH : /home/user/.julia";
}) // { python = python; }
