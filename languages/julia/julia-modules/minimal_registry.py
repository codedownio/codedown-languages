#!/usr/bin/env python3

from collections import defaultdict
import copy
import os
from pathlib import Path
import shutil
import sys
import toml
import yaml

registry_path = Path(sys.argv[1])
desired_packages_path = Path(sys.argv[2])
dependencies_path = Path(sys.argv[3])
out_path = Path(sys.argv[4])

with open(desired_packages_path, "r") as f:
  desired_packages = yaml.safe_load(f) or []

uuid_to_versions = defaultdict(list)
for pkg in desired_packages:
    uuid_to_versions[pkg["uuid"]].append(pkg["version"])

with open(dependencies_path, "r") as f:
  uuid_to_store_path = yaml.safe_load(f)

registry = toml.load(registry_path / "Registry.toml")
registry["packages"] = {k: v for k, v in registry["packages"].items() if k in uuid_to_versions}
os.makedirs(out_path)
with open(out_path / "Registry.toml", "w") as f:
    toml.dump(registry, f)


for (uuid, versions) in uuid_to_versions.items():
    if not uuid in registry["packages"]: continue

    registry_info = registry["packages"][uuid]

    # Copy some files to the minimal repo unchanged
    path = registry_info["path"]
    os.makedirs(out_path / path)
    for f in ["Compat.toml", "Deps.toml"]:
        shutil.copy2(registry_path / path / f, out_path / path)

    # Copy the Versions.toml file, trimming down to the versions we care about
    all_versions = toml.load(registry_path / path / "Versions.toml")
    versions_to_keep = {k: v for k, v in all_versions.items() if k in versions}
    for k, v in versions_to_keep.items():
        del v["nix-sha256"]
    with open(out_path / path / "Versions.toml", "w") as f:
        toml.dump(versions_to_keep, f)

    # Fill in the local store path for the repo
    if not uuid in uuid_to_store_path: continue
    package_toml = toml.load(registry_path / path / "Package.toml")
    package_toml["repo"] = "file://" + uuid_to_store_path[uuid]
    with open(out_path / path / "Package.toml", "w") as f:
        toml.dump(package_toml, f)
