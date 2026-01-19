{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.3/cpp-notebook-language-server-0.1.0.3-aarch64-linux.tar.gz";
    hash = "sha256-YK5ebj1/nc7CmFuU5wwV7R30deHt3TwgKqGg4+e1qsM=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.3/cpp-notebook-language-server-0.1.0.3-x86_64-linux.tar.gz";
    hash = "sha256-ce9Fc3m3YhFmLe8R7cHzVmJmORxEqmbLU6PpuWx7Fx8=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.3/cpp-notebook-language-server-0.1.0.3-x86_64-darwin.tar.gz";
    hash = "sha256-Zh7XLNrTYungQOYpHA9dMNyfkbx1+3mHtAWFr+NRtt4=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.3";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.3/cpp-notebook-language-server-0.1.0.3-aarch64-darwin.tar.gz";
    hash = "sha256-tv5qcWaVZdG9tEBVeDfCN8wkAYwkp5wPkPPo+FQG2pg=";
  };
  # HASHES_END
}.${system}
