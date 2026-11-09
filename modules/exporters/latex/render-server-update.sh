#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch python3

# Otherwise a failed prefetch reports success and leaves a placeholder hash behind.
set -euo pipefail

VERSION=$(nix eval --raw --expr 'import ./render-server-version.nix' --impure)

echo "Got version: $VERSION"

NAME="render-server-$VERSION"

NEW_HASHES=$(
  for system in x86_64-linux; do
    URL="https://github.com/codedownio/render-server/releases/download/v${VERSION}/render-server-${VERSION}-${system}.tar.gz"
    HASH=$(nix-prefetch fetchzip --name "$NAME" --no-stripRoot --url "$URL" 2>/dev/null)

    echo >&2 "$URL -> $HASH"

    echo "  \"$system\" = fetchzip {"
    echo "    name = \"$NAME\";"
    echo "    stripRoot = false;"
    echo "    url = \"$URL\";"
    echo "    hash = \"$HASH\";"
    echo "  };"
  done
)

py_script=$(cat <<END
from operator import indexOf
import sys

input = sys.stdin.read()

with open("./render-server.nix", 'r') as f:
  lines = f.readlines()

start_index = indexOf(map(lambda x: "HASHES_START" in x, lines), True)
end_index = indexOf(map(lambda x: "HASHES_END" in x, lines), True)

new_lines = lines[:start_index + 1] + [input] + lines[end_index:]

with open("./render-server.nix", 'w') as f:
  f.writelines(new_lines)
END
)

echo "$NEW_HASHES" | python3 -c "$py_script"
