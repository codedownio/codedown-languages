# Based on indexpackage.jl from SymbolServer.jl

module SymbolServer

using Pkg, SHA
using Base: UUID

@time "Initial includes" begin
    include("faketypes.jl")
    include("symbols.jl")
    include("utils.jl")
    include("serialize.jl")
    using .CacheStore
end

module LoadingBay end

function index_package(current_package_name, current_package_version, current_package_uuid, current_package_treehash)
    @time "Indexing package $current_package_name $current_package_version..." begin
        current_package_versionwithoutplus = replace(string(current_package_version), '+'=>'_')
        filename_with_extension = "v$(current_package_versionwithoutplus)_$current_package_treehash.jstore"

        # Load package
        m = try
            @time "Loading $current_package_name $current_package_version" begin
                LoadingBay.eval(:(import $current_package_name))
                getfield(LoadingBay, current_package_name)
            end
        catch e
            @info "Could not load package $current_package_name $current_package_version ($current_package_uuid): $e"
            return 10
        end

        # Get the symbols
        env = @time "getenvtree" getenvtree([current_package_name])
        @time "symbols" symbols(env, m, get_return_type=true)

        # Strip out paths
        @time "modify_dirs" begin
            modify_dirs(
                env[current_package_name],
                f -> modify_dir(f, pkg_src_dir(Base.loaded_modules[Base.PkgId(current_package_uuid, string(current_package_name))]), "PLACEHOLDER")
            )
        end

        # The destination path must be where SymbolServer.jl expects it
        dir = joinpath(
            ENV["out"],
            string(uppercase(string(current_package_name)[1])),
            string(current_package_name, "_", current_package_uuid),
        )

        mkpath(dir)

        @time "CacheStore.write" begin
            open(joinpath(dir, filename_with_extension), "w") do io
                CacheStore.write(io, Package(string(current_package_name), env[current_package_name], current_package_uuid, nothing))
            end
        end
    end

    # Exit with a custom error code to indicate success. This allows
    # the parent process to distinguish between a successful run and one
    # where the package exited the process.
    return 37
end

if abspath(PROGRAM_FILE) == @__FILE__
    current_package_name = Symbol(ARGS[1])
    current_package_version = VersionNumber(ARGS[2])
    current_package_uuid = UUID(ARGS[3])
    current_package_treehash = ARGS[4]

    exit(index_package(current_package_name, current_package_version, current_package_uuid, current_package_treehash))

end

end
