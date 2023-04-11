{ lib
, python3
}:

# This file contains an extra mapping from Julia packages to the Python packages they depend on.

with lib;

rec {
  packageMapping = {
    PyPlot = ["matplotlib"];
    PythonPlot = ["matplotlib"];
    SymPy = ["sympy"];
  };

  getExtraPythonPackages = names: concatMap (name: let
    allCandidates = if hasAttr name packageMapping then getAttr name packageMapping else [];
  in
    filter (x: hasAttr x python3.pkgs) allCandidates
  ) names;
}
