#!/usr/bin/env python3

from pathlib import Path
import shutil
import sys
import toml
import yaml

registry_path = Path(sys.argv[1])
desired_packages_path = Path(sys.argv[2])
out_path = Path(sys.argv[3])

with open(desired_packages_path, "r") as f:
  desired_packages = yaml.safe_load(f) or []

registry = toml.load(registry_path / "Registry.toml")

with open(out_path, "w") as f:
  f.write("{fetchgit}:\n")
  f.write("{\n")
  for pkg in desired_packages:
      uuid = pkg["uuid"]
      if not uuid in registry["packages"]: continue

      registry_info = registry["packages"][uuid]
      path = registry_info["path"]
      packageToml = toml.load(registry_path / path / "Package.toml")

      all_versions = toml.load(registry_path / path / "Versions.toml")
      if not pkg["version"] in all_versions: continue
      version_to_use = all_versions[pkg["version"]]

      repo = packageToml["repo"]
      f.write(f"""  "{uuid}" = {{
    src = fetchgit {{
      url = "{repo}";
      rev = "{version_to_use["git-tree-sha1"]}";
      sha256 = "{version_to_use["nix-sha256"]}";
    }};
    name = "{pkg["name"]}";
    version = "{pkg["version"]}";
    treehash = "{version_to_use["git-tree-sha1"]}";
  }};\n""")
  f.write("}")
