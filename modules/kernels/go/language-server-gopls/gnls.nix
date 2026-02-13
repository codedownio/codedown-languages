{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.2/go-notebook-language-server-0.1.0.2-aarch64-linux.tar.gz";
    hash = "sha256-I1qxFLjlc1E/zaC2+tcH0STlnkr1nulBb3UWm83iHcE=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.2/go-notebook-language-server-0.1.0.2-x86_64-linux.tar.gz";
    hash = "sha256-uzkTEPXQXlqusKyLDDgOd0huCer7fGyFN2FI7qHMiZg=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.2/go-notebook-language-server-0.1.0.2-x86_64-darwin.tar.gz";
    hash = "sha256-O/d0lnwL5zEQ6tLROv01k/r9OwpdJAvL3lIZk+T2/Ng=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.2/go-notebook-language-server-0.1.0.2-aarch64-darwin.tar.gz";
    hash = "sha256-HyO/oU0BNWXu2evIGHm9XlUBVI43+PerlZ9eqXp0B7M=";
  };
  # HASHES_END
}.${system}
