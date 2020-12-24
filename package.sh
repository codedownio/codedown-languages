#!/usr/bin/env bash
set -euo pipefail

rm -f nixexprs.tar.bz2
tar --exclude='.git' -cvjSf nixexprs.tar.bz2 *
