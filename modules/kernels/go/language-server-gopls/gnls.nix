{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.3.0/go-notebook-language-server-0.1.3.0-aarch64-linux.tar.gz";
    hash = "sha256-Of76J9KnwTcjxoXusbIxN9auylxbB9s5ugA9CtORVUk=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.3.0/go-notebook-language-server-0.1.3.0-x86_64-linux.tar.gz";
    hash = "sha256-+lew5MY7EavxeHVWQa/Mjn055HBUWGlI9n8RSv/drr0=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.3.0/go-notebook-language-server-0.1.3.0-x86_64-darwin.tar.gz";
    hash = "sha256-PlkgF4k2XFH0HwQAX8W+P2beb//Ag+bmzxw/lQLY7hs=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.3.0";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.3.0/go-notebook-language-server-0.1.3.0-aarch64-darwin.tar.gz";
    hash = "sha256-r8/5EY8s6DKjxWpaBr3LBK1EWBcTVXVqgzKzpv5Bl4c=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
