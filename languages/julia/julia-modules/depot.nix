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

  echo "Using registry $registry"

  mkdir -p $out/project
  export JULIA_PROJECT="$out/project"

  mkdir -p $out/depot/artifacts
  export JULIA_DEPOT_PATH="$out/depot"
  cp ${overridesToml} $out/depot/artifacts/Overrides.toml

  # These can be useful to debug problems
  # export JULIA_DEBUG=Pkg
  # export JULIA_DEBUG=loading

  export JULIA_SSL_CA_ROOTS_PATH="${cacert}/etc/ssl/certs/ca-bundle.crt"

  export JULIA_PKG_PRECOMPILE_AUTO=0

  julia -e ' \
    import Pkg
    Pkg.Registry.add(Pkg.RegistrySpec(path="${registry}"))

    # Pkg.Artifacts.load_overrides(;force=true)
    Pkg.add(${lib.generators.toJSON {} packageNames})
    Pkg.instantiate()

    if "precompile" in keys(ENV) && ENV["precompile"] != "0"
      Pkg.precompile()
    end

    # Remove the registry to save space
    Pkg.Registry.rm("General")
  '
''
