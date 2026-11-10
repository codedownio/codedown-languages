{ fetchzip
, stdenv
}:

# The prebuilt binary from the GitHub release (codedownio/lsp-export-server); lsp-export-server-update.sh
# regenerates the hashes for a new lsp-export-server-version.nix.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "lsp-export-server-0.4.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.4.1.2/lsp-export-server-0.4.1.2-aarch64-linux.tar.gz";
    hash = "sha256-Xl1gjqwwMmL66sKSuUFyoH8B3bTZgerdr7Dr7KKTNF4=";
  };
  "x86_64-linux" = fetchzip {
    name = "lsp-export-server-0.4.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.4.1.2/lsp-export-server-0.4.1.2-x86_64-linux.tar.gz";
    hash = "sha256-USAxpR5pSmrR3Kw7Huncv4nHFNTuVjzWxIr+Kfw/CTA=";
  };
  "x86_64-darwin" = fetchzip {
    name = "lsp-export-server-0.4.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.4.1.2/lsp-export-server-0.4.1.2-x86_64-darwin.tar.gz";
    hash = "sha256-h7eLA3ibe1LGQwV8FliiykkeulG2iZjCyoVjtHV+07U=";
  };
  "aarch64-darwin" = fetchzip {
    name = "lsp-export-server-0.4.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/lsp-export-server/releases/download/v0.4.1.2/lsp-export-server-0.4.1.2-aarch64-darwin.tar.gz";
    hash = "sha256-P160ZrABtp+DD4uR+GhFsKnhjZoRe2BZiXwTdQSqAxo=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
