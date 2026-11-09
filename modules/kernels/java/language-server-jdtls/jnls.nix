{ fetchzip
, stdenv
}:

# Static binaries from GitHub releases. ./jnls-update.sh rewrites these and java-parser.nix's
# source hash together.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.6.0/java-notebook-language-server-0.1.6.0-aarch64-linux.tar.gz";
    hash = "sha256-uI9EKFMKi+sqrtwj3V+/Y/fcVL4TNPU0lYik0rK10ms=";
  };
  "x86_64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.6.0/java-notebook-language-server-0.1.6.0-x86_64-linux.tar.gz";
    hash = "sha256-HOVDeTKPsWFYJntx0RdPU2GNJDUipE2kSH35YquvruI=";
  };
  "x86_64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.6.0/java-notebook-language-server-0.1.6.0-x86_64-darwin.tar.gz";
    hash = "sha256-XzAjCaWgBvtSz9I4ZbA7HP9hfcdHlMtJZaHbkz/sC0E=";
  };
  "aarch64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.6.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.6.0/java-notebook-language-server-0.1.6.0-aarch64-darwin.tar.gz";
    hash = "sha256-evUtotMh2kPllnx7jzRepkhY9lFAeWYWOO4jCnub88Y=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
