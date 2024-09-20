{ callPackage
, fetchFromGitHub
, lib
, python3
, runCommand

# , indexTransitiveDependencies ? true
# , packageNames ? []

, julia
}:

let
  # combinedStoreSeparateDerivations = let
  #   symbolStoreNix = runCommand "julia-indexes.nix" { buildInputs = [(python3.withPackages (ps: with ps; [toml pyyaml]))]; } ''
  #     indexpackage=$(find ${julia.projectAndDepot}/depot/packages/SymbolServer -name indexpackage.jl)
  #     python ${./index_packages.py} \
  #       "${julia.dependencyUuidToInfoYaml}" \
  #       '${lib.generators.toJSON {} packageNames}' \
  #       '${lib.generators.toJSON {} indexTransitiveDependencies}' \
  #       "${julia}/bin/julia" \
  #       "$indexpackage" \
  #       "$out"
  #   '';

  #   uuidToSymbolStore = callPackage symbolStoreNix {
  #     inherit julia;
  #     indexpackage = ./indexpackage.jl;
  #   };
  # in
  #   runCommand "julia-combined-store" { buildInputs = [(python3.withPackages (ps: with ps; [toml]))]; } ''
  #     python ${./combine_indices.py} \
  #       "${uuidToSymbolStore}" \
  #       "$out"
  #   '';


  juliaExpression = ''
    import SymbolServer
    module LoadingBay end
    SymbolServer.index_packages(nothing, ENV["out"], LoadingBay)
  '';

  combinedStoreThreaded = runCommand "julia-symbols-store" { buildInputs = [(python3.withPackages (ps: with ps; [toml pyyaml])) julia]; } ''
    # Convert julia.dependencyUuidToInfoYaml to TOML, which Julia can import natively
    python - <<'EOF'
    import toml
    import yaml

    with open("${julia.dependencyUuidToInfoYaml}", "r") as f:
      contents = yaml.safe_load(f)

    with open("./dependency_uuid_to_info.toml", "w") as f:
      toml.dump(contents, f)
    EOF

    mkdir home
    export HOME=$(pwd)/home

    julia  -e '${juliaExpression}';
  '';

in

# combinedStoreSeparateDerivations

combinedStoreThreaded
