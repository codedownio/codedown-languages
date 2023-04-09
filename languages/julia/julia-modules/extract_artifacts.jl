
import Base: UUID
using Pkg
using Pkg.Artifacts
using Pkg.BinaryPlatforms
using Pkg.PlatformEngines
import TOML

pkg_uuid = UUID(ARGS[1])
dir = ARGS[2]

artifacts_toml = find_artifacts_toml(dir)

if artifacts_toml == nothing
    print("")
    exit()
end

platform = platform_key_abi()

include_lazy = true

result = select_downloadable_artifacts(artifacts_toml; platform, include_lazy, pkg_uuid)

TOML.print(result);
