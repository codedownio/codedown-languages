{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.5.0/go-notebook-language-server-0.1.5.0-aarch64-linux.tar.gz";
    hash = "sha256-w8eERnwQR+wQ1Wnx6t6C1GgrrTivEwVq3WeIdsG5+wM=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.5.0/go-notebook-language-server-0.1.5.0-x86_64-linux.tar.gz";
    hash = "sha256-iXiD1PK48p2wvDNIMGTzXbh6JtNFdKrjQA6t1gSxC+Y=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.5.0/go-notebook-language-server-0.1.5.0-x86_64-darwin.tar.gz";
    hash = "sha256-ZN2dYWMrnF7CE8/x/0Ge7KY5eOTlZ0hsI7PcNoA0ZS8=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.5.0/go-notebook-language-server-0.1.5.0-aarch64-darwin.tar.gz";
    hash = "sha256-WtkjL2qX6nN8NQHJSml/kkkaNkD63HWBr1SxHav8TZE=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
