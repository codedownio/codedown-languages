
module SymbolServer

import TOML: parsefile

uuid_to_info_toml = ARGS[1]
store_path = ARGS[2]

include("indexpackage.jl")

uuid_to_info = parsefile(uuid_to_info_toml)

mkpath(store_path)

# for (uuid, info) in uuid_to_info
#   exit_code = SymbolServer.index_package(info["name"], info["version"], uuid, info["treehash"])
#   println(exit_code)
# end

!in("@stdlib", LOAD_PATH) && push!(LOAD_PATH, "@stdlib") # Make sure we can load stdlibs

conn = stdout

start_time = time_ns()

module LoadingBay end

using Pkg, SHA
using Base: UUID, write

include("faketypes.jl")
include("symbols.jl")
include("utils.jl")
include("serialize.jl")
using .CacheStore

ctx = try
    Pkg.Types.Context()
catch err
    @info "Package environment can't be read: $err"
    exit()
end
# Add some methods to check whether a package is part of the standard library and so
# won't need recaching.
if isdefined(Pkg.Types, :is_stdlib)
    is_stdlib(uuid::UUID) = Pkg.Types.is_stdlib(uuid)
else
    is_stdlib(uuid::UUID) = uuid in keys(ctx.stdlibs)
end

server = Server(store_path, ctx, Dict{UUID,Package}())

written_caches = String[] # List of caches that have already been written
toplevel_pkgs = deps(project(ctx)) # First get a list of all package UUIds that we want to cache
packages_to_load = []
# Next make sure the cache is up-to-date for all of these
for (pk_name, uuid) in toplevel_pkgs
    uuid isa UUID || (uuid = UUID(uuid))
    if !isinmanifest(ctx, uuid)
        @info "$pk_name not in manifest, skipping."
        continue
    end
    pe = frommanifest(manifest(ctx), uuid)
    cache_path = joinpath(server.storedir, SymbolServer.get_cache_path(manifest(ctx), uuid)...)

    if isfile(cache_path)
        if is_package_deved(manifest(ctx), uuid)
            try
                cached_version = open(cache_path) do io
                    CacheStore.read(io)
                end
                if sha_pkg(frommanifest(manifest(ctx), uuid)) != cached_version.sha
                    @info "Outdated sha, will recache package $pk_name ($uuid)"
                    push!(packages_to_load, uuid)
                else
                    @info "Package $pk_name ($uuid) is cached."
                end
            catch err
                @info "Couldn't load $pk_name ($uuid) from file, will recache."
            end
        else
            @info "Package $pk_name ($uuid) is cached."
        end
    else
        @info "Will cache package $pk_name ($uuid)"
        push!(packages_to_load, uuid)
    end
end

visited = Base.IdSet{Module}([Base, Core])

# Load all packages together
for (i, uuid) in enumerate(packages_to_load)
    load_package(ctx, uuid, conn, LoadingBay, round(Int, 100*(i - 1)/length(packages_to_load)))

    # XXX: The following *may* duplicate some work, but we want to make sure that interrupts of
    #      the SymbolServer process don't invalidate *all* work done (which would happen when only
    #      writing the cache files out after all packages are loaded)

    # Create image of whole package env. This creates the module structure only.
    env_symbols = getenvtree()

    # Populate the above with symbols, skipping modules that don't need caching.
    # symbols (env_symbols)
     # don't need to cache these each time...
    for (pid, m) in Base.loaded_modules
        if pid.uuid !== nothing &&
                is_stdlib(pid.uuid) &&
                isinmanifest(ctx, pid.uuid) &&
                isfile(joinpath(server.storedir, SymbolServer.get_cache_path(manifest(ctx), pid.uuid)...))
            push!(visited, m)
            delete!(env_symbols, Symbol(pid.name))
        end
    end

    symbols(env_symbols, nothing, getallns(), visited)

    # Wrap the `ModuleStore`s as `Package`s.
    for (pkg_name, cache) in env_symbols
        !isinmanifest(ctx, String(pkg_name)) && continue
        uuid = packageuuid(ctx, String(pkg_name))
        pe = frommanifest(ctx, uuid)
        server.depot[uuid] = Package(String(pkg_name), cache, uuid, sha_pkg(pe))
    end

    write_depot(server, server.context, written_caches)
end

@info "Symbol server indexing took $((time_ns() - start_time) / 1e9) seconds."

println(conn, "DONE")
close(conn)

end


mkpath(out)
