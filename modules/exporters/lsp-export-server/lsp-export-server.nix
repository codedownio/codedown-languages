{ fetchzip
, stdenv
}:

# The prebuilt binary from the GitHub release (codedownio/lsp-export-server); lsp-export-server-update.sh
# regenerates the hashes for a new lsp-export-server-version.nix.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "lsp-export-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.0.0/lsp-export-server-0.1.0.0-aarch64-linux.tar.gz";
    hash = "sha256-7LQTzqAYOHyr8IeohByxgV9BvhT8eNqO/OjpVlLCeQo=";
  };
  "x86_64-linux" = fetchzip {
    name = "lsp-export-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.0.0/lsp-export-server-0.1.0.0-x86_64-linux.tar.gz";
    hash = "sha256-Hh5sWvw3+UO6tbH5GGxhKAW+IGerRX12SBdrs30uSyY=";
  };
  "x86_64-darwin" = fetchzip {
    name = "lsp-export-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.0.0/lsp-export-server-0.1.0.0-x86_64-darwin.tar.gz";
    hash = "sha256-1btJfUmtHIaqD4bd+NPo4LSZ//cYjtPhU2aQi3hrUuM=";
  };
  "aarch64-darwin" = fetchzip {
    name = "lsp-export-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.0.0/lsp-export-server-0.1.0.0-aarch64-darwin.tar.gz";
    hash = "sha256-lBfC748fmgH5TfeIC1q/luRpgCGCAqd5UgR+2BM5m/A=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
