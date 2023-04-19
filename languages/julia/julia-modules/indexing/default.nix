{ callPackage
, lib
, python3
, runCommand
}:

packageNames:

let
  juliaWithPackages = callPackage ../. { precompile = false; };

  juliaSymbolServer = juliaWithPackages (["SymbolServer"] ++ packageNames);

  symbolStoreNix = runCommand "julia-indexes.nix" { buildInputs = [(python3.withPackages (ps: with ps; [toml pyyaml]))]; } ''
    indexpackage=$(find ${juliaSymbolServer.projectAndDepot}/depot/packages/SymbolServer -name indexpackage.jl)
    symbolServerSource=$(dirname "$indexpackage")

    python ${./index_packages.py} \
      "${(juliaWithPackages packageNames).dependencyUuidToInfoYaml}" \
      '${lib.generators.toJSON {} packageNames}' \
      "${juliaSymbolServer}/bin/julia" \
      "$symbolServerSource" \
      "$out"
  '';

  uuidToSymbolStore = callPackage symbolStoreNix {
    julia = juliaSymbolServer;
    indexpackage = ./indexpackage.jl;
  };

  combinedStore = runCommand "julia-combined-store" { buildInputs = [(python3.withPackages (ps: with ps; [toml]))]; } ''
    python ${./combine_indices.py} \
      "${uuidToSymbolStore}" \
      "$out"
  '';

in

combinedStore
