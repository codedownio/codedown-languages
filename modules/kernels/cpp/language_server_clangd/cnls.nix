{ fetchzip
, system
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.0/cpp-notebook-language-server-0.1.0.0-aarch64-linux.tar.gz";
    hash = "sha256-1WefJo6tqUJYDo/6xOufNigqyf5LiTpJ2CqiZIcmJmY=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.0/cpp-notebook-language-server-0.1.0.0-x86_64-linux.tar.gz";
    hash = "sha256-75D4oKFwYOxCg5BwCi1UqWjtf1wXU79wez8kcclWrWw=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.0/cpp-notebook-language-server-0.1.0.0-x86_64-darwin.tar.gz";
    hash = "sha256-aAco6+QlFYtx9RAOTctFHgVvXRzreZHlNHROvi0/Vxs=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.1.0.0/cpp-notebook-language-server-0.1.0.0-aarch64-darwin.tar.gz";
    hash = "sha256-ZIXrKcJQeTcyM8lXWp6+1ZCbywvBJRXoi6VC670G5Yw=";
  };
  # HASHES_END
}.${system}
