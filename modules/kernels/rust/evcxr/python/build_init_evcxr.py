#!/usr/bin/env python3

import json
from pathlib import Path
import toml
import sys

vendor_dir = Path(sys.argv[1])
packages = json.loads(sys.argv[2])
metadata_path = Path(sys.argv[3])
out = Path(sys.argv[4])


name_to_dir = {}
for subdir in (f for f in vendor_dir.resolve().glob('**/*') if f.is_dir()):
  cargo_toml_path = subdir / "Cargo.toml"
  if not cargo_toml_path.exists(): continue

  cargo_toml = toml.load(cargo_toml_path)

  if "package" in cargo_toml:
    if "name" in cargo_toml["package"]:
      name_to_dir[cargo_toml["package"]["name"]] = subdir

name_to_version = {}
with open(metadata_path, 'r') as f:
  metadata = json.load(f)
for pkg in metadata["packages"]:
  name_to_version[pkg["name"]] = pkg["version"]

with open(out, "a") as f:
  for package in packages:
    package_name = package if isinstance(package, str) else package["name"]
    features = {} if isinstance(package, str) else package.get("features", {})
    if package_name in name_to_dir:
      clauses = [f"""version = \"{name_to_version.get(package_name, "*")}\""""]
      if features:
        clauses.append(f"""features = {json.dumps(features)}""")

      clauses_joined = ", ".join(clauses)

      f.write(f""":dep {package_name} = {{ {clauses_joined} }}\n""")
