{ fetchzip
, stdenv
}:

# Static binaries from GitHub releases. ./jnls-update.sh rewrites these and java-parser.nix's
# source hash together.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.5.0/java-notebook-language-server-0.1.5.0-aarch64-linux.tar.gz";
    hash = "sha256-g6kNF9La6kBkB48aMT3n2jdqujB1Qk9x5wE1JCXwjyM=";
  };
  "x86_64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.5.0/java-notebook-language-server-0.1.5.0-x86_64-linux.tar.gz";
    hash = "sha256-tKtH3Qj1B8iyrn4vWBFvJxpa73jjYpynmxhM6mRnjKY=";
  };
  "x86_64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.5.0/java-notebook-language-server-0.1.5.0-x86_64-darwin.tar.gz";
    hash = "sha256-sbGpsZdk7bOvYGQPwTWR17IZH6Sw4dlitJ/CUMrXvhI=";
  };
  "aarch64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.5.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.5.0/java-notebook-language-server-0.1.5.0-aarch64-darwin.tar.gz";
    hash = "sha256-E7pjkLJ43p/HQC0Z/dNRGH09skLV97FxY8jUXSDlCRU=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
