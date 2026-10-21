# Must be a repl commit whose lean-toolchain matches our lean4, or it won't compile.
# Re-pick on every nixpkgs bump: in a repl checkout, `git log --follow -- lean-toolchain`,
# take the last commit on the matching version, then
# `nix store prefetch-file --unpack https://github.com/leanprover-community/repl/archive/<rev>.tar.gz`.
{
  rev = "495777293cb9cc3c62787a8c11393da1a9dd9505";
  sha256 = "sha256-0e3QHntg1kFAVVL6pQ9HH8zW8sxW0pYK2xG6ed3j6Qc=";

  # Informational, so a mismatch with lean4.version shows up in a diff.
  leanToolchain = "v4.29.0";
}
