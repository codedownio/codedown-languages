{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.3/go-notebook-language-server-0.1.0.3-aarch64-linux.tar.gz";
    hash = "sha256-q2u2Iyy5cZCnxcXppZbbahtsZ+sQi1QrKLw1JDqUeJM=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.3/go-notebook-language-server-0.1.0.3-x86_64-linux.tar.gz";
    hash = "sha256-Yq6AYnaYO1JIblz8d8oUdVWZb18trpr4JgkKcLN2IuM=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.3/go-notebook-language-server-0.1.0.3-x86_64-darwin.tar.gz";
    hash = "sha256-ovu3YFTB8vDwBp0hcFE/hV09W7vqjkC1fzKrfumXDws=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.3/go-notebook-language-server-0.1.0.3-aarch64-darwin.tar.gz";
    hash = "sha256-9n1ExuhRLioeGfdQlLTyRCr+P0+YR4yc3BLBd9W2jww=";
  };
  # HASHES_END
}.${system}
