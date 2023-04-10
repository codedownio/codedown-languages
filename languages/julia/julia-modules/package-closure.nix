{ lib
, runCommand
, julia
, augmentedRegistry
, packageNames
}:

runCommand "julia-package.yml" { buildInputs = [julia]; } ''
  mkdir home
  export HOME=$(pwd)/home
  export OUT="$out"

  echo "Resolving Julia packages with the following inputs"
  echo "Julia: ${julia}"
  echo "Registry: ${augmentedRegistry}"
  echo "Packages: ${lib.generators.toJSON {} packageNames}"

  # Prevent a warning where Julia tries to download package server info
  export JULIA_PKG_SERVER=""

  julia -e ' \
    import Pkg
    Pkg.Registry.add(Pkg.RegistrySpec(path="${augmentedRegistry}"))

    import Pkg.API: handle_package_input!
    import Pkg.Types: PackageSpec, VersionSpec, PRESERVE_NONE, project_deps_resolve!, registry_resolve!, stdlib_resolve!, ensure_resolved
    import Pkg.Operations: assert_can_add, _resolve, update_package_add

    input = unique(${lib.generators.toJSON {} packageNames})
    pkgs = [PackageSpec(pkg) for pkg in input]
    foreach(handle_package_input!, pkgs)

    ctx = Pkg.Types.Context()

    project_deps_resolve!(ctx.env, pkgs)
    registry_resolve!(ctx.registries, pkgs)
    stdlib_resolve!(pkgs)
    ensure_resolved(ctx, ctx.env.manifest, pkgs, registry=true)
    assert_can_add(ctx, pkgs)

    # load manifest data
    for (i, pkg) in pairs(pkgs)
        entry = Pkg.Types.manifest_info(ctx.env.manifest, pkg.uuid)
        is_dep = any(uuid -> uuid == pkg.uuid, [uuid for (name, uuid) in ctx.env.project.deps])
        pkgs[i] = update_package_add(ctx, pkg, entry, is_dep)
    end

    foreach(pkg -> ctx.env.project.deps[pkg.name] = pkg.uuid, pkgs) # update set of deps

    # resolve
    pkgs, deps_map = _resolve(ctx.io, ctx.env, ctx.registries, pkgs, PRESERVE_NONE, ctx.julia_version)

    open(ENV["OUT"], "w") do io
      for spec in pkgs
          println(io, "- name: " * spec.name)
          println(io, "  uuid: " * string(spec.uuid))
          println(io, "  version: " * string(spec.version))
      end
    end
  ';
''
