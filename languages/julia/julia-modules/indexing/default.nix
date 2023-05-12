{ callPackage
, lib
, python3
, runCommand

, indexTransitiveDependencies ? true
}:

packageNames:

let
  julia = callPackage ../. {
    precompile = true;
    makeTransitiveDependenciesImportable = true;
  } (packageNames ++ ["SymbolServer"]);

  # juliaSymbolServer = callPackage ../. {} ["SymbolServer"];

  combinedStoreSeparateDerivations = let
    symbolStoreNix = runCommand "julia-indexes.nix" { buildInputs = [(python3.withPackages (ps: with ps; [toml pyyaml]))]; } ''
      indexpackage=$(find ${julia.projectAndDepot}/depot/packages/SymbolServer -name indexpackage.jl)
      symbolServerSource=$(dirname "$indexpackage")

      python ${./index_packages.py} \
        "${julia.dependencyUuidToInfoYaml}" \
        '${lib.generators.toJSON {} packageNames}' \
        '${lib.generators.toJSON {} indexTransitiveDependencies}' \
        "${julia}/bin/julia" \
        "$symbolServerSource" \
        "$out"
    '';

    uuidToSymbolStore = callPackage symbolStoreNix {
      inherit julia;
      indexpackage = ./indexpackage.jl;
    };
  in
    runCommand "julia-combined-store" { buildInputs = [(python3.withPackages (ps: with ps; [toml]))]; } ''
      python ${./combine_indices.py} \
        "${uuidToSymbolStore}" \
        "$out"
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

    symbolServerSource=$(dirname "$(find ${julia.projectAndDepot}/depot/packages/SymbolServer -name indexpackage.jl)")

    cp -r $symbolServerSource/. .
    chmod u+w ./indexpackage.jl
    cp ${./indexpackage.jl} ./indexpackage.jl
    cp ${./index_all_packages.jl} ./index_all_packages.jl

    mkdir home
    export HOME=$(pwd)/home

    julia ./index_all_packages.jl \
      "./dependency_uuid_to_info.toml" \
      "$out"
  '';

in

# combinedStoreSeparateDerivations

combinedStoreThreaded
