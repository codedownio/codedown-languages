{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.6";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.6/cpp-notebook-language-server-0.1.0.6-aarch64-linux.tar.gz";
    hash = "sha256-WKjD/Efy+9wD4UgOhGC80MhGegr9tt407Z7OnwxdaiI=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.6";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.6/cpp-notebook-language-server-0.1.0.6-x86_64-linux.tar.gz";
    hash = "sha256-0WAuHS5nTfibfqze9rMXrBQAHsaaPyHcBBlR1GdY7w0=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.6";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.6/cpp-notebook-language-server-0.1.0.6-x86_64-darwin.tar.gz";
    hash = "sha256-ZI2R14901OdDbwpJ7q0WDDMxMPRgRHrfygm0vdcplqo=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.6";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.6/cpp-notebook-language-server-0.1.0.6-aarch64-darwin.tar.gz";
    hash = "sha256-UGB6tVuWlMOxskNFdICaVTurdbd95auAMg7Ty4Iawng=";
  };
  # HASHES_END
}.${system}
