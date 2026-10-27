{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.4.0/go-notebook-language-server-0.1.4.0-aarch64-linux.tar.gz";
    hash = "sha256-TDKpkWhVmtGH/vaCDIlLXvKxfI8P8I4QSMdjteclN6k=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.4.0/go-notebook-language-server-0.1.4.0-x86_64-linux.tar.gz";
    hash = "sha256-5jdY4sTWio84swuZ3N5InDToPFzQKjdu1ks8qZwSepw=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.4.0/go-notebook-language-server-0.1.4.0-x86_64-darwin.tar.gz";
    hash = "sha256-+ErpKkumw37rXvVjs8wY72hTH50dN+kSipkpwC3IIL0=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.4.0/go-notebook-language-server-0.1.4.0-aarch64-darwin.tar.gz";
    hash = "sha256-6qk5IlxWh6UkWDAQnSn7Ify2MMuQTvJLgjHRCqx/sDA=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
