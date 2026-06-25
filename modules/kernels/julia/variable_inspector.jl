# Variable inspector for the Julia (IJulia) Jupyter kernel.
#
# Everything lives in a single module, `CodedownVariableInspector`, so the inspector
# adds exactly one name to Main (and modules are filtered out of its own listing).
# The frontend runs this file once at kernel startup, then calls
# `CodedownVariableInspector.dict_list()` and `CodedownVariableInspector.inspect("<name>")`.
#
# JSON is always available (a dependency of IJulia). DataFrames is accessed
# dynamically so this script loads even when DataFrames isn't imported, and is only
# touched when a DataFrame is actually present.

module CodedownVariableInspector

import JSON

const MAX_CONTENT = 150
const MAX_ROWS = 10000

_typestr(x) = string(typeof(x))
_typename(x) = string(nameof(typeof(x)))

_is_matrix(x) = x isa AbstractArray ? ndims(x) <= 2 : _typename(x) == "DataFrame"

function _size_bytes(x)
    try
        return Int(Base.summarysize(x))
    catch
        return nothing
    end
end

function _shape(x)
    try
        x isa AbstractArray && return collect(Int.(size(x)))
        _typename(x) == "DataFrame" && return collect(Int.(size(x)))
        x isa AbstractString && return nothing
        if x isa Union{AbstractDict,AbstractSet,Tuple} || applicable(length, x)
            return [Int(length(x))]
        end
    catch
    end
    return nothing
end

function _truncate(s::AbstractString, n::Int)
    s = replace(s, r"[\r\n]+" => " ")
    return length(s) > n ? string(first(s, n), " ...") : s
end

# Access DataFrames functions dynamically so we don't depend on it being imported.
_df_names(x) = try
    [string(c) for c in getproperty(Main, :names)(x)]
catch
    String[]
end

function _content(x)
    s = try
        _typename(x) == "DataFrame" ? "Columns: " * join(_df_names(x), ", ") : repr(x)
    catch
        "<unprintable>"
    end
    return _truncate(s, MAX_CONTENT)
end

const _OWN = Symbol("CodedownVariableInspector")

function _user_vars()
    out = Symbol[]
    for s in names(Main; all = false, imported = false)
        ss = string(s)
        (s === _OWN || startswith(ss, "#") || startswith(ss, "CodedownVariableInspector")) && continue
        s in (:Main, :Base, :Core, :InteractiveUtils, :IJulia) && continue
        isdefined(Main, s) || continue
        val = getfield(Main, s)
        (val isa Module || val isa Function || val isa DataType || val isa UnionAll) && continue
        push!(out, s)
    end
    return out
end

function dict_list()
    d = Dict{String,Any}()
    for s in _user_vars()
        val = getfield(Main, s)
        d[string(s)] = Dict{String,Any}(
            "type"     => _typestr(val),
            "size"     => _size_bytes(val),
            "shape"    => _shape(val),
            "content"  => _content(val),
            "isMatrix" => _is_matrix(val),
        )
    end
    println(JSON.json(d))
    return nothing
end

function _table(x)
    if _typename(x) == "DataFrame"
        cols = _df_names(x)
        nrow = try size(x, 1) catch; 0 end
        n = min(nrow, MAX_ROWS)
        getidx = getproperty(Main, :getindex)
        data = Any[Any[getidx(x, i, c) for c in 1:length(cols)] for i in 1:n]
        return Dict("columns" => cols, "data" => data)
    elseif x isa AbstractMatrix
        n = min(size(x, 1), MAX_ROWS)
        cols = [string(j) for j in 1:size(x, 2)]
        data = Any[Any[x[i, j] for j in 1:size(x, 2)] for i in 1:n]
        return Dict("columns" => cols, "data" => data)
    elseif x isa AbstractVector
        n = min(length(x), MAX_ROWS)
        return Dict("columns" => ["value"], "data" => Any[Any[x[i]] for i in 1:n])
    end
    return nothing
end

function inspect(name::AbstractString)
    s = Symbol(name)
    val = isdefined(Main, s) ? getfield(Main, s) : nothing
    ismat = _is_matrix(val)
    fullcontent = try
        sprint(show, "text/plain", val)
    catch
        try
            repr(val)
        catch
            "<unprintable>"
        end
    end
    out = Dict{String,Any}(
        "name"     => name,
        "type"     => _typestr(val),
        "size"     => _size_bytes(val),
        "shape"    => _shape(val),
        "isMatrix" => ismat,
        "content"  => fullcontent,
        "table"    => ismat ? _table(val) : nothing,
    )
    println(JSON.json(out))
    return nothing
end

end # module CodedownVariableInspector
