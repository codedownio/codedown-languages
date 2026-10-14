{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.1/cpp-notebook-language-server-0.2.3.1-aarch64-linux.tar.gz";
    hash = "sha256-k5KqDZXnOeKqZunemM0zz2Th7+c3y8vo/CounIrX468=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.1/cpp-notebook-language-server-0.2.3.1-x86_64-linux.tar.gz";
    hash = "sha256-GuCKu4+J3bnFi8uGuz5cR6YX5gnPXWulhzc25ko93aw=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.1/cpp-notebook-language-server-0.2.3.1-x86_64-darwin.tar.gz";
    hash = "sha256-ro9KiFyW0WC5nLAEGd9KAVzM90u0nG71HjxZST3y9cY=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.3.1";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.3.1/cpp-notebook-language-server-0.2.3.1-aarch64-darwin.tar.gz";
    hash = "sha256-XcH5+jRJjHzRHv1ejxzjMQSu24ZVjdGbqq0kg2JY9Ew=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
