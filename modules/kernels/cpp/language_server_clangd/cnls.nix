{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.5.0/cpp-notebook-language-server-0.2.5.0-aarch64-linux.tar.gz";
    hash = "sha256-3kfAk/9PpOexJAdXisrRhZnxq+AWZ7/bOWa+t8FkTIw=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.5.0/cpp-notebook-language-server-0.2.5.0-x86_64-linux.tar.gz";
    hash = "sha256-iY53wHe/6K1AB6QTbpZgyzwq/lkiE3FpX+OPnJmUneY=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.5.0/cpp-notebook-language-server-0.2.5.0-x86_64-darwin.tar.gz";
    hash = "sha256-qpSlK7lSLxKZbBSWSW4KsT3111wqsisInMEIXmzQW2Y=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.5.0/cpp-notebook-language-server-0.2.5.0-aarch64-darwin.tar.gz";
    hash = "sha256-ksgt9WUmZWqC030ei6t5z710zP455MZlAlJaVWjoW3U=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
