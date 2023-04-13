{ lib
, callPackage
, runCommand
, fetchFromGitHub
, fetchgit
, fetchurl
, git
, makeWrapper
, writeTextFile
, python3
, stdenv

, julia
, extraLibs ? []
, precompile ? true
, setDefaultDepot ? true
, makeWrapperArgs ? ""
}:

packageNames:

let
  # Special registry which is equal to JuliaRegistries/General, but every Versions.toml
  # entry is augmented with a Nix sha256 hash
  augmentedRegistry = callPackage ./registry.nix {};

  # Some Julia packages require access to Python. Provide a Nixpkgs version so it
  # doesn't try to install its own.
  pythonToUse = let
    extraPythonPackages = ((callPackage ./extra-python-packages.nix { inherit python3; }).getExtraPythonPackages packageNames);
  in (if extraPythonPackages == [] then python3 else python3.withPackages (ps:
    (map (pkg: lib.getAttr pkg ps) extraPythonPackages))
  );

  # Start by wrapping Julia so it has access to Python and any other extra libs.
  # Also, prevent various packages (CondaPkg.jl, PythonCall.jl) from trying to do network calls.
  juliaWrapped = runCommand "julia-${julia.version}-wrapped" { buildInputs = [makeWrapper]; inherit makeWrapperArgs; } ''
    mkdir -p $out/bin
    makeWrapper ${julia}/bin/julia $out/bin/julia \
      --suffix LD_LIBRARY_PATH : "${lib.makeLibraryPath extraLibs}" \
      --set PYTHONHOME "${pythonToUse}" \
      --prefix PYTHONPATH : "${pythonToUse}/${pythonToUse.sitePackages}" \
      --set PYTHON ${pythonToUse}/bin/python $makeWrapperArgs \
      --set JULIA_CONDAPKG_OFFLINE yes \
      --set JULIA_CONDAPKG_BACKEND Null \
      --set JULIA_PYTHONCALL_EXE "@PyCall"
  '';

  # If our closure ends up with certain packages, add others.
  packageImplications = {
    # Because we want to put PythonCall in PyCall mode so it doesn't try to download
    # Python packages
    PythonCall = ["PyCall"];
  };

  # Invoke Julia resolution logic to determine the full dependency closure
  closureYaml = callPackage ./package-closure.nix {
    inherit augmentedRegistry julia packageNames packageImplications;
  };

  # Generate a Nix file consisting of a map from dependency UUID --> package info with fetchgit call:
  # {
  #   "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3" = {
  #     src = fetchgit {...};
  #     name = "...";
  #     version = "...";
  #     treehash = "...";
  #   };
  #   ...
  # }
  dependencies = runCommand "julia-sources.nix" { buildInputs = [(python3.withPackages (ps: with ps; [toml pyyaml]))]; } ''
    python ${./sources_nix.py} \
      "${augmentedRegistry}" \
      "${closureYaml}" \
      "$out"
  '';

  # Import the Nix file from the previous step (IFD) and turn each dependency repo into
  # a dummy Git repository, as Julia expects. Format the results as a YAML map from
  # dependency UUID -> Nix store location:
  # {
  #   "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3":"/nix/store/...-NaNMath.jl-0877504",
  #   ...
  # }
  dependencyUuidToRepo = writeTextFile {
    name = "dependency-uuid-to-repo.yml";
    text = lib.generators.toYAML {} (lib.mapAttrs repoify (import dependencies { inherit fetchgit; }));
  };
  repoify = uuid: info:
    runCommand ''julia-${info.name}-${info.version}'' {buildInputs = [git];} ''
      mkdir -p $out
      cp -r ${info.src}/. $out
      cd $out
      git init
      git add . -f
      git config user.email "julia2nix@localhost"
      git config user.name "julia2nix"
      git commit -m "Dummy commit"
    '';

  # Given the augmented registry, closure info yaml, and dependency path yaml, construct a complete
  # Julia registry containing all the necessary packages
  minimalRegistry = runCommand "minimal-julia-registry" { buildInputs = [(python3.withPackages (ps: with ps; [toml pyyaml]))]; } ''
    python ${./minimal_registry.py} \
      "${augmentedRegistry}" \
      "${closureYaml}" \
      "${dependencyUuidToRepo}" \
      "$out"
  '';

  # Next, deal with artifacts. Scan each artifacts file individually and generate a Nix file that
  # produces the desired Overrides.toml.
  artifactsNix = runCommand "julia-artifacts.nix" { buildInputs = [(python3.withPackages (ps: with ps; [toml pyyaml]))]; } ''
    python ${./extract_artifacts.py} \
      "${dependencyUuidToRepo}" \
      "${juliaWrapped}/bin/julia" \
      "${./extract_artifacts.jl}" \
      "$out"
  '';

  # Import the artifacts Nix to build Overrides.toml (IFD)
  overridesTomlRaw = import artifactsNix { inherit fetchurl stdenv writeTextFile; };
  overridesToml = runCommand "Overrides.toml" { buildInputs = [(python3.withPackages (ps: with ps; [toml]))]; } ''
    python ${./dedup_overrides.py} \
      "${overridesTomlRaw}" \
      "$out"
  '';

  # Build a Julia project and depot. The project contains Project.toml/Manifest.toml, while the
  # depot contains package build products (including the precompiled libraries, if precompile=true)
  projectAndDepot = callPackage ./depot.nix {
    inherit closureYaml extraLibs overridesToml packageNames packageImplications precompile;
    julia = juliaWrapped;
    registry = minimalRegistry;
  };

in

runCommand "julia-${julia.version}-env" {
  buildInputs = [makeWrapper];

  inherit julia;
  inherit juliaWrapped;

  # Expose the steps we used along the way in case the user wants to use them, for example to build
  # expressions and build them separately to avoid IFD.
  inherit dependencies;
  dependencyUuidToInfo = writeTextFile {
    name = "dependency-uuid-to-info.yml";
    text = lib.generators.toYAML {} (import dependencies { inherit fetchgit; });
  };
  inherit dependencyUuidToRepo;
  inherit minimalRegistry;
  inherit artifactsNix;
  inherit overridesToml;
  inherit overridesTomlRaw;
  inherit projectAndDepot;
} (''
  mkdir -p $out/bin
  makeWrapper ${juliaWrapped}/bin/julia $out/bin/julia \
    --suffix JULIA_DEPOT_PATH : "${projectAndDepot}/depot" \
    --set-default JULIA_PROJECT "${projectAndDepot}/project"
'' + lib.optionalString setDefaultDepot ''
  sed -i '2 i\JULIA_DEPOT_PATH=''${JULIA_DEPOT_PATH-"$HOME/.julia"}' $out/bin/julia
'')
