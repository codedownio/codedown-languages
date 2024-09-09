#!/usr/bin/env python3

import json
from pathlib import Path
import toml
import sys

vendor_dir = Path(sys.argv[1])
package_names = json.loads(sys.argv[2])
cargo_lock_path = Path(sys.argv[3])
out = Path(sys.argv[4])


name_to_dir = {}
for subdir in (f for f in vendor_dir.resolve().glob('**/*') if f.is_dir()):
  cargo_toml_path = subdir / "Cargo.toml"
  if not cargo_toml_path.exists(): continue

  cargo_toml = toml.load(cargo_toml_path)

  if "package" in cargo_toml:
    if "name" in cargo_toml["package"]:
      name_to_dir[cargo_toml["package"]["name"]] = subdir

with open(out, "a") as f:
  for package in package_names:
    package_name = package if isinstance(package, str) else package["name"]
    settings = {} if isinstance(package, str) else package.get("settings", {})
    if package_name in name_to_dir:
      features_clause = ""
      if "features" in settings:
        features_clause = f""", features = {json.dumps(settings["features"])}"""

      f.write(f""":dep {package_name} = {{ path = "{str(name_to_dir[package_name])}"{features_clause} }}\n""")
