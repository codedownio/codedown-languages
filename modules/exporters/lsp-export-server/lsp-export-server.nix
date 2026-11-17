{ fetchzip
, stdenv
}:

# The prebuilt binary from the GitHub release (codedownio/lsp-export-server); lsp-export-server-update.sh
# regenerates the hashes for a new lsp-export-server-version.nix.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "lsp-export-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.2.0/lsp-export-server-0.1.2.0-aarch64-linux.tar.gz";
    hash = "sha256-azu2His6tv0BR/L7OeanXHaf9R3giGDXJ6HNG5rwfDs=";
  };
  "x86_64-linux" = fetchzip {
    name = "lsp-export-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.2.0/lsp-export-server-0.1.2.0-x86_64-linux.tar.gz";
    hash = "sha256-Zuy2P9gWQecyDHZaGNFRwAvO/4S+dK/4qHmo1i03NhE=";
  };
  "x86_64-darwin" = fetchzip {
    name = "lsp-export-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.2.0/lsp-export-server-0.1.2.0-x86_64-darwin.tar.gz";
    hash = "sha256-vKzp3yMKXXX6JR2/ulhcCNiXa8end58TnCj7OWSHfCE=";
  };
  "aarch64-darwin" = fetchzip {
    name = "lsp-export-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.2.0/lsp-export-server-0.1.2.0-aarch64-darwin.tar.gz";
    hash = "sha256-9PTYxBs/tbqJR5JA+rymwB1CGlLgQ39Rf/FQnpU/ZZQ=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
