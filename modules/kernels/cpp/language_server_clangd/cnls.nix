{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.6.0/cpp-notebook-language-server-0.2.6.0-aarch64-linux.tar.gz";
    hash = "sha256-bGcKwUyGTa8aHDfQpRpsLB+2TCY2PXgdiooTDhkd5D4=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.6.0/cpp-notebook-language-server-0.2.6.0-x86_64-linux.tar.gz";
    hash = "sha256-V7Ath0fAc4kA3xysg4pVWYBXjucPjRm8LekCBs8SR0k=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.6.0/cpp-notebook-language-server-0.2.6.0-x86_64-darwin.tar.gz";
    hash = "sha256-3T9vteAMyVjVye45UMLw2+ji+46KsomMM977wFddr0g=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.6.0/cpp-notebook-language-server-0.2.6.0-aarch64-darwin.tar.gz";
    hash = "sha256-azMhguetA6A5gQUeI6jw9zGxCTWNMockPgYBrFluY+I=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
