{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.7.0/go-notebook-language-server-0.1.7.0-aarch64-linux.tar.gz";
    hash = "sha256-pzO6hgciVTX8PeRFUUqHaI8WY1jXMIyTpFsk1uZ7TOg=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.7.0/go-notebook-language-server-0.1.7.0-x86_64-linux.tar.gz";
    hash = "sha256-RXomJOqmKGr+IqeWrRr004Fi2dShC6D9Ar1bWYfV3kI=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.7.0/go-notebook-language-server-0.1.7.0-x86_64-darwin.tar.gz";
    hash = "sha256-mtYPH3oZD3XDDGEf2JHCG4mnZMm+YTkZKmETY8wPyas=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.7.0/go-notebook-language-server-0.1.7.0-aarch64-darwin.tar.gz";
    hash = "sha256-xkf8mUl/ZRxpKprXU0BijD9qoigaCiG0nMpP50gpn74=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
