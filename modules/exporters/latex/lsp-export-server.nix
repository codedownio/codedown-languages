{ fetchzip
, stdenv
}:

# A static binary from the GitHub release (codedownio/lsp-export-server); lsp-export-server-update.sh
# regenerates the hashes for a new lsp-export-server-version.nix.
{
  # HASHES_START
  "x86_64-linux" = fetchzip {
    name = "lsp-export-server-0.2.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.2.0.0/lsp-export-server-0.2.0.0-x86_64-linux.tar.gz";
    hash = "sha256-qXMLtflq0x25qSp1CQhlAsEZeeKvsL86XIMrlo+KCgI=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
