{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.7.0/cpp-notebook-language-server-0.2.7.0-aarch64-linux.tar.gz";
    hash = "sha256-2ygux5MlmpiGiok2ttdQbf6TFUVQOI1tL7182vNRa1I=";
  };
  "x86_64-linux" = fetchzip {
    name = "cpp-notebook-language-server-0.2.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.7.0/cpp-notebook-language-server-0.2.7.0-x86_64-linux.tar.gz";
    hash = "sha256-Wg5eu0u43jIeytP7fApL79NP68J5FVgNoTkQAIE8mOk=";
  };
  "x86_64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.7.0/cpp-notebook-language-server-0.2.7.0-x86_64-darwin.tar.gz";
    hash = "sha256-Di5cQLYBCoy0GxRRn58tk+qu+TbgVLmPOF4t690qNNs=";
  };
  "aarch64-darwin" = fetchzip {
    name = "cpp-notebook-language-server-0.2.7.0";
    stripRoot = false;
    url = "https://github.com/codedownio/cpp-notebook-language-server/releases/download/v0.2.7.0/cpp-notebook-language-server-0.2.7.0-aarch64-darwin.tar.gz";
    hash = "sha256-d2LbFZA7pNxnwjfLhILklShtasN1scIjFZc78Wrm8g0=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
