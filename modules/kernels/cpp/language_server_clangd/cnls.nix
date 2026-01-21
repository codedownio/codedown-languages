{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.5";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.5/cpp-notebook-language-server-0.1.0.5-aarch64-linux.tar.gz";
    hash = "sha256-tpOkXXMrs+Pq3o8Kc1rGrQCJYfpHxsVkc7TRt8n8ORA=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.5";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.5/cpp-notebook-language-server-0.1.0.5-x86_64-linux.tar.gz";
    hash = "sha256-rngXx/1B+XixmRaiwZDhfeNXrzqI5oGfuSTJWswj35g=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.5";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.5/cpp-notebook-language-server-0.1.0.5-x86_64-darwin.tar.gz";
    hash = "sha256-jGegIG4yjaNq9HNXu+8+NV7WWDoO/H6R+7HK84NA6jI=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.5";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.5/cpp-notebook-language-server-0.1.0.5-aarch64-darwin.tar.gz";
    hash = "sha256-cufFvLV0YQZ5kpN7zKMMlh3PxRsMa7vUDLNb4EhQPEY=";
  };
  # HASHES_END
}.${system}
