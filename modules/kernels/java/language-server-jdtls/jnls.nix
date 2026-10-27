{ fetchzip
, stdenv
}:

# Static binaries from GitHub releases. ./jnls-update.sh rewrites these and java-parser.nix's
# source hash together.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.3.0/java-notebook-language-server-0.1.3.0-aarch64-linux.tar.gz";
    hash = "sha256-4hsD+yCV//PR0OoGq1HcbJMUGkWiOoCNu3e2O1QLO6Q=";
  };
  "x86_64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.3.0/java-notebook-language-server-0.1.3.0-x86_64-linux.tar.gz";
    hash = "sha256-J06NRWgckT3xox+dKqO3bIoP870JVYc0U+wBRwL2EDw=";
  };
  "x86_64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.3.0/java-notebook-language-server-0.1.3.0-x86_64-darwin.tar.gz";
    hash = "sha256-Y1mnvOc/zfLEufeEhAPSsBozxKH3lzIk0ZwSnrz8/08=";
  };
  "aarch64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.3.0/java-notebook-language-server-0.1.3.0-aarch64-darwin.tar.gz";
    hash = "sha256-ah1bUaajRyfIx9odP9ZvbxRVY+fFK6hXlwiyyUjVYgg=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
