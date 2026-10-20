# The leanprover-community/repl commit to build. It must be one whose `lean-toolchain`
# matches the `lean4` in our nixpkgs pin -- the repl is Lean source, so a mismatch is a
# compile error, not a runtime warning.
#
# To re-pick after a nixpkgs bump: `nix eval nixpkgs#lean4.version` for the new Lean
# version, then in a repl checkout
#
#     git log --oneline --follow -- lean-toolchain
#
# and take the *last* commit whose `lean-toolchain` is `leanprover/lean4:v<that version>`
# (the commit after it bumps to the next toolchain). Patch releases are fine: v4.29.0 is
# the newest toolchain repl ever pinned in the 4.29 series and it builds against 4.29.1.
#
# Then update the hash with
#
#     nix store prefetch-file --unpack \
#       https://github.com/leanprover-community/repl/archive/<rev>.tar.gz
{
  rev = "495777293cb9cc3c62787a8c11393da1a9dd9505";
  sha256 = "sha256-0e3QHntg1kFAVVL6pQ9HH8zW8sxW0pYK2xG6ed3j6Qc=";

  # Informational: the toolchain this commit declares, so a mismatch with lean4.version is
  # obvious in a diff.
  leanToolchain = "v4.29.0";
}
