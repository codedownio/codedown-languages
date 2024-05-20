#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch

VERSION=$(nix eval --raw --expr 'import ./hnls-version.nix' --impure)

echo "Got version: $VERSION"

for ghc in ghc810 ghc90 ghc92 ghc94 ghc96 ghc98; do
  URL="https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${VERSION}/haskell-notebook-language-server-${VERSION}-${ghc}-x86_64-linux.tar.gz"
  HASH=$(nix-prefetch fetchzip --url "$URL" 2>/dev/null)

  echo "      \"$ghc\" = prebuilt \"$ghc\" (fetchzip {"
  echo "        url = mkUrl \"$ghc\";"
  echo "        sha256 = \"$HASH\";"
  echo "      });"
done
