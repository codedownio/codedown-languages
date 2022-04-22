#!/usr/bin/env bash

cd "$(mktemp -d)"

>&2 echo "Using temp dir: $(pwd)"

KERNEL="$1"
CODE="$2"

# cat <<EOF > notebook.ipynb
# {
#     "nbformat_minor": 2,
#     "nbformat": 4,
#     "cells": [
#         {
#             "metadata": {},
#             "source": ["$CODE"],
#             "cell_type": "code",
#             "execution_count": null,
#             "outputs": []
#         }
#     ],
#     "metadata": {}
# }
# EOF


# jupyter execute --kernel "$KERNEL" notebook.ipynb
# jupyter run --kernel "$KERNEL" notebook.ipynb
# jq '.cells[0].outputs' < notebook.ipynb


echo "$CODE" | jupyter run --kernel "$KERNEL"
