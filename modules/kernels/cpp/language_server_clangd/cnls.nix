{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.4/cpp-notebook-language-server-0.1.0.4-aarch64-linux.tar.gz";
    hash = "sha256-zqdPMYZSYPlGbsZSPXHWOuUPmeN/mpdv+ZZaiAFI/sg=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.4/cpp-notebook-language-server-0.1.0.4-x86_64-linux.tar.gz";
    hash = "sha256-gZDFJfzZZRkiWCXuuiURg6PrE6WI1iNnmrWW023y4jY=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.4/cpp-notebook-language-server-0.1.0.4-x86_64-darwin.tar.gz";
    hash = "sha256-b2TpkAWeAIciCKZTCjTIXcKG+pYfNCnPNnJLbNbuoa0=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.4/cpp-notebook-language-server-0.1.0.4-aarch64-darwin.tar.gz";
    hash = "sha256-Sk4n6N6UHjI53T/OMqzpEm4B5gi3hmvkZxXu+zXYmxE=";
  };
  # HASHES_END
}.${system}
