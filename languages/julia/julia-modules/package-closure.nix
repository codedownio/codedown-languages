{ lib
, runCommand
, julia
, augmentedRegistry
, packageNames
}:

let
  # The specific package resolution code depends on the Julia version
  # These are pretty similar and could be combined to reduce duplication
  resolveCode = if lib.versionOlder julia.version "1.7" then resolveCode1_6 else resolveCode1_8;

  resolveCode1_6 = ''
    import Pkg.API: check_package_name
    import Pkg.Types: Context, Context!, PackageSpec, PRESERVE_NONE, manifest_info, project_deps_resolve!, registry_resolve!, stdlib_resolve!, ensure_resolved
    import Pkg.Operations: _resolve, assert_can_add, is_dep, update_package_add

    input = unique(${lib.generators.toJSON {} packageNames})
    pkgs = [PackageSpec(pkg) for pkg in input]

    foreach(pkg -> check_package_name(pkg.name, :add), pkgs)
    pkgs = deepcopy(pkgs)  # deepcopy for avoid mutating PackageSpec members
    ctx = Context()
    Context!(ctx)

    project_deps_resolve!(ctx, pkgs)
    registry_resolve!(ctx, pkgs)
    stdlib_resolve!(pkgs)
    ensure_resolved(ctx, pkgs, registry=true)

    assert_can_add(ctx, pkgs)

    # load manifest data
    for (i, pkg) in pairs(pkgs)
        entry = manifest_info(ctx, pkg.uuid)
        pkgs[i] = update_package_add(ctx, pkg, entry, is_dep(ctx, pkg))
    end

    foreach(pkg -> ctx.env.project.deps[pkg.name] = pkg.uuid, pkgs) # update set of deps

    # resolve
    pkgs, deps_map = _resolve(ctx, pkgs, PRESERVE_NONE)
'';

  resolveCode1_8 = ''
    import Pkg.API: handle_package_input!
    import Pkg.Types: Context, PackageSpec, PRESERVE_NONE, project_deps_resolve!, registry_resolve!, stdlib_resolve!, ensure_resolved
    import Pkg.Operations: _resolve, assert_can_add, update_package_add

    input = unique(${lib.generators.toJSON {} packageNames})
    pkgs = [PackageSpec(pkg) for pkg in input]
    foreach(handle_package_input!, pkgs)

    ctx = Context()

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
'';

in

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

    ${resolveCode}

    open(ENV["OUT"], "w") do io
      for spec in pkgs
          println(io, "- name: " * spec.name)
          println(io, "  uuid: " * string(spec.uuid))
          println(io, "  version: " * string(spec.version))
      end
    end
  ';
''
