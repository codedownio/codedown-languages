{ fetchzip
, stdenv
}:

# A static binary from the GitHub release (codedownio/render-server); render-server-update.sh
# regenerates the hashes for a new render-server-version.nix.
{
  # HASHES_START
  "x86_64-linux" = fetchzip {
    name = "render-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/render-server/releases/download/v0.1.0.0/render-server-0.1.0.0-x86_64-linux.tar.gz";
    hash = "sha256-kRAj4jy2K7wpH9gqOilllfzIuqrBDkvdJgowSrFi9fM=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
