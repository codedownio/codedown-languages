{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.9.0/cpp-notebook-language-server-0.2.9.0-aarch64-linux.tar.gz";
    hash = "sha256-Aer4xhxllAsEAb+LFB2pLOO7mRhz01oUM81D6qSendQ=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.9.0/cpp-notebook-language-server-0.2.9.0-x86_64-linux.tar.gz";
    hash = "sha256-0wWaztZGDvz7rbHK5bl9mCVP2NrfJZotRcWa7dCQ5yI=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.9.0/cpp-notebook-language-server-0.2.9.0-x86_64-darwin.tar.gz";
    hash = "sha256-pDGf6uOgBNuLOfydKEk7KcYV1oXPdjI75KIvcNqhpHY=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.9.0/cpp-notebook-language-server-0.2.9.0-aarch64-darwin.tar.gz";
    hash = "sha256-TUrm640DcAD+fefKbnRyJg+So5z/7YfOyl79yUTV6uE=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
