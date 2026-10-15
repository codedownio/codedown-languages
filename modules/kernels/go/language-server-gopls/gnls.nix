{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.2/go-notebook-language-server-0.1.1.2-aarch64-linux.tar.gz";
    hash = "sha256-FdjezaexbM8R+yxfx9bdZYTrblAL8hvPFbXwysIuqV8=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.2/go-notebook-language-server-0.1.1.2-x86_64-linux.tar.gz";
    hash = "sha256-slIcmSas4Pmy7G6o/aSTMMcheGxBGM/gGJHfquSqn8A=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.2/go-notebook-language-server-0.1.1.2-x86_64-darwin.tar.gz";
    hash = "sha256-AITXGaD2O7KFTx/HSChybXxjqgHAS9AVUvwOZOw+IMY=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.1.2";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.2/go-notebook-language-server-0.1.1.2-aarch64-darwin.tar.gz";
    hash = "sha256-sNlWO4jDUaAto8cBEvYz+HblenezBZjqeg7x79EyLw8=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
