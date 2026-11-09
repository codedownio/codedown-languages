{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.6.0/go-notebook-language-server-0.1.6.0-aarch64-linux.tar.gz";
    hash = "sha256-jcejwYfdxf3sRCItYCd80wxNiSjNB76qOq77XlSt1hk=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.6.0/go-notebook-language-server-0.1.6.0-x86_64-linux.tar.gz";
    hash = "sha256-Vl3BQRWro4Ht7DrN0Rm9mMjr74I0FObVxtxs6Xm+4cA=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.6.0/go-notebook-language-server-0.1.6.0-x86_64-darwin.tar.gz";
    hash = "sha256-+CJ3enNyI3lSpJGO0Vdqrxmxy8liptS+S+PRTwz127E=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.6.0/go-notebook-language-server-0.1.6.0-aarch64-darwin.tar.gz";
    hash = "sha256-x2S9ZgAGVTgC5SFKse5YXm/RvQfhNLc6YX7K+fWf/4E=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
