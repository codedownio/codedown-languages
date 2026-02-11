{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.0/go-notebook-language-server-0.1.0.0-aarch64-linux.tar.gz";
    hash = "";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.0/go-notebook-language-server-0.1.0.0-x86_64-linux.tar.gz";
    hash = "";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.0/go-notebook-language-server-0.1.0.0-x86_64-darwin.tar.gz";
    hash = "";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.0/go-notebook-language-server-0.1.0.0-aarch64-darwin.tar.gz";
    hash = "";
  };
  # HASHES_END
}.${system}
