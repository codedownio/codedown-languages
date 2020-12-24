#!/usr/bin/env bash
set -euo pipefail

rm -f nixexprs.tar.bz2
tmp=$(mktemp -d)
tar --exclude='.git' --transform='s,^\.,base,' -cvjSf $tmp/nixexprs.tar.bz2 .
tar --exclude='.git' --transform='s,^\.,base,' -cvJSf $tmp/nixexprs.tar.xz .
echo "Wrote archives to $tmp/"
