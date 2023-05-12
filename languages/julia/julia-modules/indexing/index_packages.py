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
julia_symbolserver_src = Path(sys.argv[5])
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

    if not index_transitive_deps:
      if info["name"] not in package_names: return ""

    lines = []

    lines.append(f"""{uuid} = "${{runCommand "{info["name"]}-{info["name"]}-symbols" {{ buildInputs = [julia]; }} ''
  mkdir -p $out
  cp -r {julia_symbolserver_src}/. .
  chmod u+w ./indexpackage.jl
  cp ${{indexpackage}} ./indexpackage.jl
  set +e
  julia ./indexpackage.jl {info["name"]} "{info["version"]}" "{uuid}" "{info["treehash"]}"
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
