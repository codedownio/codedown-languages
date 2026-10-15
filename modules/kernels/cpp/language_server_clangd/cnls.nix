{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.2/cpp-notebook-language-server-0.2.3.2-aarch64-linux.tar.gz";
    hash = "sha256-OvkJ7y6IXouxqW0NbJ9PwcbOYs3Gq5GeEOWBvAxeSP0=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.2/cpp-notebook-language-server-0.2.3.2-x86_64-linux.tar.gz";
    hash = "sha256-nhyuQNDqrSjjW/VuY7Iud2drrp6szN9+JY6cG0yrQFc=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.2/cpp-notebook-language-server-0.2.3.2-x86_64-darwin.tar.gz";
    hash = "sha256-eQIB2WP4oCYYFC9GuL9U76SbtWvbp7VJMWG7H0dYxMM=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.2/cpp-notebook-language-server-0.2.3.2-aarch64-darwin.tar.gz";
    hash = "sha256-SDlGvczhevCUEAlWBywrxKyFiHoJqxA0cUBtbJQWZUQ=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
