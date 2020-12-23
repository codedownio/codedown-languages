#!/bin/bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPTDIR

COMMIT_HASH=$(git ls-remote git@github.com:codedownio/markdown-spellcheck-lsp.git | grep refs/heads/master | awk '{print $1}')

cat <<EOF > node-packages.json
[
  {"markdown-spellcheck-lsp": "git+https://github.com/codedownio/markdown-spellcheck-lsp.git#${COMMIT_HASH}"}
]
EOF


sed -i "s|markdown-spellcheck-lsp.git#[^\"]*|markdown-spellcheck-lsp.git#$COMMIT_HASH|g" ../markdown-spellcheck-lsp.nix
