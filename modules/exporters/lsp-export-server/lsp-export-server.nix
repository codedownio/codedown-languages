{ fetchzip
, stdenv
}:

# A static binary from the GitHub release (codedownio/lsp-export-server); lsp-export-server-update.sh
# regenerates the hashes for a new lsp-export-server-version.nix.
{
  # HASHES_START
  "x86_64-linux" = fetchzip {
    name = "lsp-export-server-0.4.1.1";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.4.1.1/lsp-export-server-0.4.1.1-x86_64-linux.tar.gz";
    hash = "sha256-CSuwjLJoWkQC9nBnDnWjWfVnMp7wUjCXKJIXffMt6VY=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
