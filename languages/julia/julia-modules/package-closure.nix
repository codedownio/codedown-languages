{ lib
, julia
, python3
, runCommand

, augmentedRegistry
, packageNames
, packageImplications
}:

let
  # The specific package resolution code depends on the Julia version
  # These are pretty similar and could be combined to reduce duplication
  resolveCode = if lib.versionOlder julia.version "1.7" then resolveCode1_6 else resolveCode1_8;

  resolveCode1_6 = ''
    import Pkg.API: check_package_name
    import Pkg.Types: Context!, PRESERVE_NONE, manifest_info, project_deps_resolve!, registry_resolve!, stdlib_resolve!, ensure_resolved
    import Pkg.Operations: _resolve, assert_can_add, is_dep, update_package_add

    foreach(pkg -> check_package_name(pkg.name, :add), pkgs)
    pkgs = deepcopy(pkgs)  # deepcopy for avoid mutating PackageSpec members
    Context!(ctx)

    project_deps_resolve!(ctx, pkgs)
    registry_resolve!(ctx, pkgs)
    stdlib_resolve!(pkgs)
    ensure_resolved(ctx, pkgs, registry=true)

    assert_can_add(ctx, pkgs)

    for (i, pkg) in pairs(pkgs)
        entry = manifest_info(ctx, pkg.uuid)
        pkgs[i] = update_package_add(ctx, pkg, entry, is_dep(ctx, pkg))
    end

    foreach(pkg -> ctx.env.project.deps[pkg.name] = pkg.uuid, pkgs)

    pkgs, deps_map = _resolve(ctx, pkgs, PRESERVE_NONE)
'';

  resolveCode1_8 = ''
    import Pkg.API: handle_package_input!
    import Pkg.Types: PRESERVE_NONE, project_deps_resolve!, registry_resolve!, stdlib_resolve!, ensure_resolved
    import Pkg.Operations: _resolve, assert_can_add, update_package_add

    foreach(handle_package_input!, pkgs)

    project_deps_resolve!(ctx.env, pkgs)
    registry_resolve!(ctx.registries, pkgs)
    stdlib_resolve!(pkgs)
    ensure_resolved(ctx, ctx.env.manifest, pkgs, registry=true)

    assert_can_add(ctx, pkgs)

    for (i, pkg) in pairs(pkgs)
        entry = Pkg.Types.manifest_info(ctx.env.manifest, pkg.uuid)
        is_dep = any(uuid -> uuid == pkg.uuid, [uuid for (name, uuid) in ctx.env.project.deps])
        pkgs[i] = update_package_add(ctx, pkg, entry, is_dep)
    end

    foreach(pkg -> ctx.env.project.deps[pkg.name] = pkg.uuid, pkgs)

    pkgs, deps_map = _resolve(ctx.io, ctx.env, ctx.registries, pkgs, PRESERVE_NONE, ctx.julia_version)
'';

  juliaExpression = packageNames: ''
    import Pkg
    Pkg.Registry.add(Pkg.RegistrySpec(path="${augmentedRegistry}"))

    import Pkg.Types: Context, PackageSpec

    input = ${lib.generators.toJSON {} packageNames}

    if isfile("extra_package_names.txt")
      append!(input, readlines("extra_package_names.txt"))
    end

    input = unique(input)

    println("Resolving packages: " * join(input, " "))

    pkgs = [PackageSpec(pkg) for pkg in input]

    ctx = Context()

    ${resolveCode}

    open(ENV["out"], "w") do io
      for spec in pkgs
          println(io, "- name: " * spec.name)
          println(io, "  uuid: " * string(spec.uuid))
          println(io, "  version: " * string(spec.version))
      end
    end
  '';
in

runCommand "julia-package.yml" { buildInputs = [julia (python3.withPackages (ps: with ps; [pyyaml]))]; } ''
  mkdir home
  export HOME=$(pwd)/home

  echo "Resolving Julia packages with the following inputs"
  echo "Julia: ${julia}"
  echo "Registry: ${augmentedRegistry}"

  # Prevent a warning where Julia tries to download package server info
  export JULIA_PKG_SERVER=""

  julia -e '${juliaExpression packageNames}';

  # See if we need to add any extra package names based on the closure
  # and the packageImplications
  python ${./find_package_implications.py} "$out" '${lib.generators.toJSON {} packageImplications}' extra_package_names.txt

  if [ -f extra_package_names.txt ]; then
    echo "Re-resolving with additional package names"
    julia -e '${juliaExpression packageNames}';
  fi
''
