{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.1/cpp-notebook-language-server-0.1.0.1-aarch64-linux.tar.gz";
    hash = "sha256-HA+V1s7VdKztyptT7TRnH2SpohaPhfo/NTfO0iFt4jY=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.1/cpp-notebook-language-server-0.1.0.1-x86_64-linux.tar.gz";
    hash = "sha256-N1/FK14ILBfjQyN7fO2DH5IYlNk2AmzVJLn3dIMkLR8=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.1/cpp-notebook-language-server-0.1.0.1-x86_64-darwin.tar.gz";
    hash = "sha256-Hf9x95lh6p1sKHTuSEZeyve6UumH/S6lAFgITzBeH7Q=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.1/cpp-notebook-language-server-0.1.0.1-aarch64-darwin.tar.gz";
    hash = "sha256-E5rHMQ4yCSjclxPOIPJirYR5odU94I/Kvtok8SOE0Hg=";
  };
  # HASHES_END
}.${system}
