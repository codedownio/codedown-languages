{ fetchzip
, stdenv
}:

# A static binary from the GitHub release (codedownio/lsp-export-server); lsp-export-server-update.sh
# regenerates the hashes for a new lsp-export-server-version.nix.
{
  # HASHES_START
  "x86_64-linux" = fetchzip {
    name = "lsp-export-server-0.4.1.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.4.1.0/lsp-export-server-0.4.1.0-x86_64-linux.tar.gz";
    hash = "sha256-vR1qSuopon9q42Q2DZvshg5J0Lm6TWCf1JF+fe1sYYc=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
