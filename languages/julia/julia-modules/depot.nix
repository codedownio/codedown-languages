{ lib
, runCommand
, cacert
, curl
, julia
, extraLibs
, overridesToml
, packageNames
, precompile
, registry
}:

runCommand "julia-depot" {
    buildInputs = [curl julia] ++ extraLibs;
    inherit precompile registry;
  } ''
  export HOME=$(pwd)

  echo "Building Julia depot and project with the following inputs"
  echo "Julia: ${julia}"
  echo "Registry: $registry"
  echo "Overrides ${overridesToml}"

  mkdir -p $out/project
  export JULIA_PROJECT="$out/project"

  mkdir -p $out/depot/artifacts
  export JULIA_DEPOT_PATH="$out/depot"
  cp ${overridesToml} $out/depot/artifacts/Overrides.toml

  # These can be useful to debug problems
  # export JULIA_DEBUG=Pkg
  # export JULIA_DEBUG=loading

  export JULIA_SSL_CA_ROOTS_PATH="${cacert}/etc/ssl/certs/ca-bundle.crt"

  # Only precompile if configured to below
  export JULIA_PKG_PRECOMPILE_AUTO=0

  # Prevent a warning where Julia tries to download package server info
  export JULIA_PKG_SERVER=""

  julia -e ' \
    import Pkg
    Pkg.Registry.add(Pkg.RegistrySpec(path="${registry}"))

    # Pkg.Artifacts.load_overrides(;force=true)
    Pkg.add(unique(${lib.generators.toJSON {} packageNames}))
    Pkg.instantiate()

    if "precompile" in keys(ENV) && ENV["precompile"] != "0"
      Pkg.precompile()
    end

    # Remove the registry to save space
    Pkg.Registry.rm("General")
  '
''
