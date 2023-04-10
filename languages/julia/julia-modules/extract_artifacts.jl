
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

if VERSION.major == 1
    if VERSION.minor < 7
      # Older Julia doesn't provide select_downloadable_artifacts, so gather the artifacts the old-fashioned way
      artifact_dict = Pkg.Artifacts.load_artifacts_toml(artifacts_toml; pkg_uuid=pkg_uuid)

      results = Dict()
      for name in keys(artifact_dict)
          # Get the metadata about this name for the requested platform
          meta = artifact_meta(name, artifact_dict, artifacts_toml; platform=platform)

          # If there are no instances of this name for the desired platform, skip it
          meta === nothing && continue

          results[name] = meta
      end
      TOML.print(results)
    else
      TOML.print(select_downloadable_artifacts(artifacts_toml; platform, include_lazy, pkg_uuid))
    end
end
