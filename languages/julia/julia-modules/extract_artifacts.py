
from pathlib import Path
import multiprocessing
import subprocess
import sys
import toml
import yaml

dependencies_path = Path(sys.argv[1])
julia_path = Path(sys.argv[2])
extract_artifacts_script = Path(sys.argv[3])
out_path = Path(sys.argv[4])

with open(dependencies_path, "r") as f:
  dependencies = yaml.safe_load(f)

with open(out_path, "w") as f:
  f.write("{ fetchurl, stdenv, writeTextFile }:\n\n")
  f.write("writeTextFile {\n")
  f.write("  name = \"Overrides.toml\";\n")
  f.write("  text = ''\n")

  def process_item(item):
    uuid, src = item
    lines = []
    artifacts = toml.loads(subprocess.check_output([julia_path, extract_artifacts_script, uuid, src]).decode())
    if not artifacts: return

    for artifact_name, details in artifacts.items():
      if len(details["download"]) == 0: return
      download = details["download"][0]
      url = download["url"]
      sha256 = download["sha256"]

      git_tree_sha1 = details["git-tree-sha1"]

      lines.append('%s = "${stdenv.mkDerivation %s}"' % (git_tree_sha1, f"""{{
    name = "{artifact_name}";
    src = fetchurl {{ url = "{url}"; sha256 = "{sha256}"; }};
    sourceRoot = ".";
    dontConfigure = true;
    dontBuild = true;
    installPhase = "cp -r . $out";
    dontFixup = true;
  }}"""))
      lines.append("")

    return "\n".join(lines)

  with multiprocessing.Pool(10) as pool:
    results = pool.map(process_item, dependencies.items())
    for s in (x for x in results if x is not None):
      f.write(s)

  f.write(f"""
  '';
}}\n""")
