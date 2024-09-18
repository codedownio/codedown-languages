

using Base: UUID
using SymbolServer

@info "Indexing standard library"

env = SymbolServer.getenvtree()
SymbolServer.symbols(env, get_return_type=true)

for k in keys(env)
  open(joinpath(ENV["out"], "$(string(k)).jstore"), "w") do io
      SymbolServer.CacheStore.write(io, SymbolServer.Package(string(k), env[k], UUID("7073ff75-c697-5162-941a-fcdaad2a7d2a"), nothing))
  end
end
