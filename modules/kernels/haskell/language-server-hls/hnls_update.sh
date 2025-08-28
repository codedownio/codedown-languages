#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch python3

VERSION=$(nix eval --raw --expr 'import ./hnls-version.nix' --impure)

echo "Got version: $VERSION"

NEW_HASHES=$(
  for system in x86_64-linux x86_64-darwin aarch64-darwin; do
    for ghc in ghc92 ghc94 ghc96 ghc98 ghc910 ghc912; do
      URL="https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${VERSION}/haskell-notebook-language-server-${VERSION}-${ghc}-${system}.tar.gz"
      HASH=$(nix-prefetch fetchzip --url "$URL" 2>/dev/null)

      echo >&2 "$URL -> $HASH"

      echo "      \"$ghc-$system\" = prebuilt \"$ghc-$system\" (fetchzip {"
      echo "        url = mkUrl \"$ghc\" \"$system\";"
      echo "        sha256 = \"$HASH\";"
      echo "      });"
    done
  done
)

py_script=$(cat <<END
from operator import indexOf
import sys

input = sys.stdin.read()

with open("./hnls.nix", 'r') as f:
  lines = f.readlines()

start_index = indexOf(map(lambda x: "HASHES_START" in x, lines), True)
end_index = indexOf(map(lambda x: "HASHES_END" in x, lines), True)

with open("./hnls.nix", 'w') as f:
  f.write("".join(lines[0:(start_index+1)]) + input + "".join(lines[end_index:]))

END
)

echo "$NEW_HASHES" | python -c "$py_script"
