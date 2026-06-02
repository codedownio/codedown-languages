{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.4/go-notebook-language-server-0.1.0.4-aarch64-linux.tar.gz";
    hash = "sha256-GZG3miAdBqLKat4arB1isjMKKpnRl8m2LOYtKUBT8oM=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.4/go-notebook-language-server-0.1.0.4-x86_64-linux.tar.gz";
    hash = "sha256-0hMofbVtiAmIRhWihDBYZ9pXy9yfpiqeBLX+L0PwreE=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.4/go-notebook-language-server-0.1.0.4-x86_64-darwin.tar.gz";
    hash = "sha256-8z71FB5mV4s9gW5QCBdPE6a3jRRx/OX/6JA/hX3T2Zc=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.0.4";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.0.4/go-notebook-language-server-0.1.0.4-aarch64-darwin.tar.gz";
    hash = "sha256-cAcGaKANex4kMnYD4JZb+JeEcidkNIxj8CF3pns03gE=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
