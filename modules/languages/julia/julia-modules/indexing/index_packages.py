#!/usr/bin/env python3

import json
from pathlib import Path
import multiprocessing
import sys
import yaml

dependencies_path = Path(sys.argv[1])
package_names = json.loads(sys.argv[2])
index_transitive_deps = json.loads(sys.argv[3])
julia_path = Path(sys.argv[4])
indexpackage = Path(sys.argv[5])
out_path = Path(sys.argv[6])

with open(dependencies_path, "r") as f:
  dependencies = yaml.safe_load(f)

with open(out_path, "w") as f:
  f.write("{ indexpackage, julia, runCommand, writeTextFile }:\n\n")
  f.write("writeTextFile {\n")
  f.write("  name = \"Indexes.toml\";\n")
  f.write("  text = ''\n")

  def process_item(item):
    uuid, info = item

    julia_expression = f"""
using Base: UUID

import SymbolServer

module LoadingBay end

name = Symbol("{info["name"]}")
version = VersionNumber("{info["version"]}")
uuid = UUID("{uuid}")

m = try
    @time "Loading $name $version" begin
        LoadingBay.eval(:(import $name))
        getfield(LoadingBay, name)
    end
catch e
    @info "Could not load package $name $version ($uuid): $e"
    return 10
end

exit(SymbolServer.index_package(name, version, uuid, "{info["treehash"]}", "{out_path}", m))
"""

    if not index_transitive_deps:
      if info["name"] not in package_names: return ""

    lines = []

    lines.append(f"""{uuid} = "${{runCommand "{info["name"]}-{info["name"]}-symbols" {{ buildInputs = [julia]; }} ''
  mkdir -p $out

  set +e

  julia -e '{julia_expression}'
  code="$?"
  if [ $code -eq 37 ]; then
    exit 0
  elif [ $code -eq 10 ]; then
    echo "Failed to load {info["name"]} {info["version"]}"
  else
    echo "Unexpected exit code: $?"
    exit 1
  fi
''}}"\n""")

    return "\n".join(lines)

  with multiprocessing.Pool(10) as pool:
    results = pool.map(process_item, dependencies.items())
    for s in (x for x in results if x is not None):
      f.write(s)

  f.write(f"""
  '';
}}\n""")
