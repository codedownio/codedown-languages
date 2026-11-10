{ fetchzip
, stdenv
}:

# Static binaries from GitHub releases. ./jnls-update.sh rewrites these and java-parser.nix's
# source hash together.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.7.0/java-notebook-language-server-0.1.7.0-aarch64-linux.tar.gz";
    hash = "sha256-chkpPHNFJ7rs7vE706n7QocxXAedpLvbbtFeO9vgNtk=";
  };
  "x86_64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.7.0/java-notebook-language-server-0.1.7.0-x86_64-linux.tar.gz";
    hash = "sha256-RqNHUEX4dcw0ogPFEFzglqDvB8OuoK2WasbzMdIkLg4=";
  };
  "x86_64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.7.0/java-notebook-language-server-0.1.7.0-x86_64-darwin.tar.gz";
    hash = "sha256-vK2CGheVsP0+0szgjzKYY4vUG1U1XY3PE35397zZCJU=";
  };
  "aarch64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.7.0/java-notebook-language-server-0.1.7.0-aarch64-darwin.tar.gz";
    hash = "sha256-5dHxi0w2596fGWG5mqrWuKFyYzAHVdd64F90RSZPSMY=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
