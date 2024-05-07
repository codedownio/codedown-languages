#!/usr/bin/env sh

VERSION=0.3.2.0

for ghc in ghc8107 ghc902 ghc928 ghc948 ghc964 ghc982; do
  URL="https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${VERSION}/haskell-notebook-language-server-${VERSION}-${ghc}-x86_64-linux.tar.gz"
  HASH=$(nix-prefetch-url "$URL" --unpack 2>/dev/null)

  echo "      \"$ghc\" = prebuilt (fetchzip {"
  echo "        url = \"$URL\";"
  echo "        sha256 = \"$HASH\";"
  echo "      });"
done
