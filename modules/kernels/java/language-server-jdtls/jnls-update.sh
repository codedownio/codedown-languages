#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch python3

# Otherwise a failed prefetch reports success and leaves a placeholder hash behind.
set -euo pipefail

VERSION=$(nix eval --raw --expr 'import ./jnls-version.nix' --impure)

echo "Got version: $VERSION"

NAME="java-notebook-language-server-$VERSION"

NEW_HASHES=$(
  for system in aarch64-linux x86_64-linux x86_64-darwin aarch64-darwin; do
    URL="https://github.com/codedownio/java-notebook-language-server/releases/download/v${VERSION}/java-notebook-language-server-${VERSION}-${system}.tar.gz"
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

with open("./jnls.nix", 'r') as f:
  lines = f.readlines()

start_index = indexOf(map(lambda x: "HASHES_START" in x, lines), True)
end_index = indexOf(map(lambda x: "HASHES_END" in x, lines), True)

with open("./jnls.nix", 'w') as f:
  f.write("".join(lines[0:(start_index+1)]) + input + "".join(lines[end_index:]))

END
)

echo "$NEW_HASHES" | python -c "$py_script"

# java-parser builds from source, so it needs the repo's hash at the same tag. Prefetching the
# archive rather than using nix-prefetch's fetchFromGitHub, whose defaults set both rev and tag
# (rejected by current Nixpkgs); --unpack gives the NAR hash fetchFromGitHub wants.
echo ""
echo "Prefetching the source tree for java-parser..."

SRC_URL="https://github.com/codedownio/java-notebook-language-server/archive/v${VERSION}.tar.gz"
SRC_BASE32=$(nix-prefetch-url --unpack --type sha256 "$SRC_URL" 2>/dev/null | tail -1)

if [[ -z "$SRC_BASE32" ]]; then
  echo >&2 "Failed to prefetch $SRC_URL"
  exit 1
fi

SRC_HASH=$(nix hash convert --hash-algo sha256 --to sri "$SRC_BASE32")
echo >&2 "source at v$VERSION -> $SRC_HASH"

python3 - "$SRC_HASH" <<'END'
import re
import sys

new_hash = sys.argv[1]
if not new_hash:
    sys.exit("nix-prefetch-url returned nothing for the source tree")

with open("./java-parser.nix") as f:
    contents = f.read()

updated, count = re.subn(
    r"(# SOURCE_HASH[^\n]*\n\s*hash = )[^;]*;",
    lambda m: m.group(1) + '"%s";' % new_hash,
    contents,
)
if count != 1:
    sys.exit("expected exactly one # SOURCE_HASH marker in java-parser.nix, found %d" % count)

with open("./java-parser.nix", "w") as f:
    f.write(updated)
END

echo "Updated java-parser.nix"
