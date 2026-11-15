{ fetchzip
, stdenv
}:

# The prebuilt binary from the GitHub release (codedownio/lsp-export-server); lsp-export-server-update.sh
# regenerates the hashes for a new lsp-export-server-version.nix.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "lsp-export-server-0.1.1.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.1.0/lsp-export-server-0.1.1.0-aarch64-linux.tar.gz";
    hash = "sha256-0sbzfVOIGA8MNIg8LTFz3zi1td0LBgLi0D/P8soglgE=";
  };
  "x86_64-linux" = fetchzip {
    name = "lsp-export-server-0.1.1.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.1.0/lsp-export-server-0.1.1.0-x86_64-linux.tar.gz";
    hash = "sha256-SkAkPGCrn7FJi4kY6QtUKA6DEcgwCkv4eQyfk2ZjUfY=";
  };
  "x86_64-darwin" = fetchzip {
    name = "lsp-export-server-0.1.1.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.1.0/lsp-export-server-0.1.1.0-x86_64-darwin.tar.gz";
    hash = "sha256-HrC23iXIBGtq3NiQHNpCkcpDFZHZwlus9aHU3MsHKQ8=";
  };
  "aarch64-darwin" = fetchzip {
    name = "lsp-export-server-0.1.1.0";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.1.1.0/lsp-export-server-0.1.1.0-aarch64-darwin.tar.gz";
    hash = "sha256-WVx/R7q1UfEvjj22EfoBIRJHYjhoF7EV/OZfbKC4KWA=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
