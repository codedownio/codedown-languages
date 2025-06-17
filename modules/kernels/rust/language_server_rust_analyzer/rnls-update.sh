#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch python3

VERSION=$(nix eval --raw --expr 'import ./rnls-version.nix' --impure)

echo "Got version: $VERSION"

NAME="rust-notebook-language-server-$VERSION"

NEW_HASHES=$(
  for system in aarch64-linux x86_64-linux x86_64-darwin aarch64-darwin; do
    URL="https://github.com/codedownio/rust-notebook-language-server/releases/download/v${VERSION}/rust-notebook-language-server-${VERSION}-${system}.tar.gz"
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

with open("./rnls.nix", 'r') as f:
  lines = f.readlines()

start_index = indexOf(map(lambda x: "HASHES_START" in x, lines), True)
end_index = indexOf(map(lambda x: "HASHES_END" in x, lines), True)

with open("./rnls.nix", 'w') as f:
  f.write("".join(lines[0:(start_index+1)]) + input + "".join(lines[end_index:]))

END
)

echo "$NEW_HASHES" | python -c "$py_script"
