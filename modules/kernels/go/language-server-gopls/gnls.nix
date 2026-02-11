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
    hash = "sha256-qSVpm3fhPEV6AJ6G3qIOSeViWRnKL0/SNI1BteYMQsk=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.0/go-notebook-language-server-0.1.0.0-x86_64-linux.tar.gz";
    hash = "sha256-QGr5hvljdNM21mbYUHcO9vUXimBh238TzFm8yX9YNXs=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.0/go-notebook-language-server-0.1.0.0-x86_64-darwin.tar.gz";
    hash = "sha256-/OP9P3VlMXRJVAECo5c+ChEQU7T/j1F1oQ8l2K4wMYc=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.0/go-notebook-language-server-0.1.0.0-aarch64-darwin.tar.gz";
    hash = "sha256-grgF1Hj6mMxj4hEAhfJyf8xn8PwMHIac1hqXrR/0xK4=";
  };
  # HASHES_END
}.${system}
