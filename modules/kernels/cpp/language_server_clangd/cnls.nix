{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.8.0/cpp-notebook-language-server-0.2.8.0-aarch64-linux.tar.gz";
    hash = "sha256-kmELzrjBKWtrGk1374U4/miVffgsifCStCI2gV1ZRTo=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.8.0/cpp-notebook-language-server-0.2.8.0-x86_64-linux.tar.gz";
    hash = "sha256-+/avKOKj1OVL1V03ZTSGW9JVl0jYUVewvwdsbTCCaJU=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.8.0/cpp-notebook-language-server-0.2.8.0-x86_64-darwin.tar.gz";
    hash = "sha256-uEn72HgJXZsdAz2yYXJHMLrBhV/vPwcXD4Mlvk3z7B8=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.8.0/cpp-notebook-language-server-0.2.8.0-aarch64-darwin.tar.gz";
    hash = "sha256-oeDRE2Gt1dwBfG9YmcCE7pcHp/1gqkYALALZy8l0sIw=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
