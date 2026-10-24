{ fetchzip
, stdenv
}:

# Static binaries from GitHub releases. ./jnls-update.sh rewrites these and java-parser.nix's
# source hash together.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.2.0/java-notebook-language-server-0.1.2.0-aarch64-linux.tar.gz";
    hash = "sha256-R91euvEiawOf5rE6bMV9wn0N+WkEmfE8+ln53zG/Rm8=";
  };
  "x86_64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.2.0/java-notebook-language-server-0.1.2.0-x86_64-linux.tar.gz";
    hash = "sha256-8shEnA5BRGJ4Hf5+uEtdby6u8Zg+Zt9F1yofUlTCg4w=";
  };
  "x86_64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.2.0/java-notebook-language-server-0.1.2.0-x86_64-darwin.tar.gz";
    hash = "sha256-XuPO8fltdem79fvj2EY947TYqBCAwntiQOnkN00atAU=";
  };
  "aarch64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.2.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.2.0/java-notebook-language-server-0.1.2.0-aarch64-darwin.tar.gz";
    hash = "sha256-X4ZnmHREgIzr7cHFqHeH2wzJbEq+6s94DdBJWnKFzy4=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
