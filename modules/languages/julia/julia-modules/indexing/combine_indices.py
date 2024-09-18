
import os
from pathlib import Path
import subprocess
import sys
import toml

uuid_to_store = toml.load(Path(sys.argv[1]))
out_path = Path(sys.argv[2])

os.makedirs(out_path)

for path in uuid_to_store.values():
    process_result = subprocess.run(["find", ".", "-name", "*.jstore"], cwd=path, stdout=subprocess.PIPE)
    files = [x for x in process_result.stdout.decode().split("\n") if x]
    for f in files:
        target = out_path / Path(f)
        if target.exists(): continue

        os.makedirs(target.parent)
        os.symlink(Path(path) / Path(f), target)
