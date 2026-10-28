{ fetchzip
, stdenv
}:

# Static binaries from GitHub releases. ./jnls-update.sh rewrites these and java-parser.nix's
# source hash together.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.4.0/java-notebook-language-server-0.1.4.0-aarch64-linux.tar.gz";
    hash = "sha256-/SiKqJ5YN08I+hGyeuQ1RY8pVpOVGTjknPVKwvJ3AlI=";
  };
  "x86_64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.4.0/java-notebook-language-server-0.1.4.0-x86_64-linux.tar.gz";
    hash = "sha256-orQEvrRUm5R7gktitDRlUZ7Xf8+HfeWuIMQ5CQm5gYU=";
  };
  "x86_64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.4.0/java-notebook-language-server-0.1.4.0-x86_64-darwin.tar.gz";
    hash = "sha256-RyNxQBxR7vaQuTzKx5Us+MM/bYCZ4PmNxs6T3PNnKq0=";
  };
  "aarch64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.4.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.4.0/java-notebook-language-server-0.1.4.0-aarch64-darwin.tar.gz";
    hash = "sha256-6Wi0Pzqp6dsWiPnazxuFFP1D4giYqIHPkxP3wByjYrA=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
