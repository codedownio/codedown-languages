{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.2/cpp-notebook-language-server-0.1.0.2-aarch64-linux.tar.gz";
    hash = "sha256-1/RkOXXxgL6YiJ54T82Vii+MC1tbpXSeVIWFzaTLBbk=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.2/cpp-notebook-language-server-0.1.0.2-x86_64-linux.tar.gz";
    hash = "sha256-CwdcEOsM3Mwy6F9B81U4an8gWrq/sdc2N809ni4SWMo=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.2/cpp-notebook-language-server-0.1.0.2-x86_64-darwin.tar.gz";
    hash = "sha256-GyxLufGx4bCoFFhQ19E9GTEBXZCvEbMzYM103IDkjJ8=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.2";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.2/cpp-notebook-language-server-0.1.0.2-aarch64-darwin.tar.gz";
    hash = "sha256-mV2PBPoSjA68g2uoKw2t+dBrerAv5cg3Z42i9n1kN80=";
  };
  # HASHES_END
}.${system}
