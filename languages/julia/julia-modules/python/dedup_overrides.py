
from pathlib import Path
import sys
import toml

overrides_path = Path(sys.argv[1])
out_path = Path(sys.argv[2])

# Remove duplicate keys in the input TOML file. This works because
# we know the input format is always rows of the form
# HASH = PATH
# TOML decoders don't like this, but we can simply parse it by splitting
# by whitespace.

with open(overrides_path, "r") as f:
  lines = f.readlines()

rows = [line.split(" ") for line in lines if "=" in line]

deduped = {}
for row in rows:
  deduped[row[0]] = row

with open(out_path, "w") as f:
  for v in deduped.values():
    f.write(" ".join(v))
