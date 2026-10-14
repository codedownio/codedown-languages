{ fetchzip
, stdenv
}:

# Fetch a static binary from GitHub releases
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.1.1";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.1/go-notebook-language-server-0.1.1.1-aarch64-linux.tar.gz";
    hash = "sha256-ylTxtZiPoh0QZEmQy8nlCaek+zamHbIKad9rzwiJCIk=";
  };
  "x86_64-linux" = fetchzip {
    name = "go-notebook-language-server-0.1.1.1";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.1/go-notebook-language-server-0.1.1.1-x86_64-linux.tar.gz";
    hash = "sha256-r3VzgujPwol5rt662iGmwZN7/PsIC+L65M5K7yp40DE=";
  };
  "x86_64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.1.1";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.1/go-notebook-language-server-0.1.1.1-x86_64-darwin.tar.gz";
    hash = "sha256-tjTZhmdWmzkmBvK0rdgnF7QDCfMMqf3RSe/WkfVdJFw=";
  };
  "aarch64-darwin" = fetchzip {
    name = "go-notebook-language-server-0.1.1.1";
    stripRoot = false;
    url = "https://github.com/codedownio/go-notebook-language-server/releases/download/v0.1.1.1/go-notebook-language-server-0.1.1.1-aarch64-darwin.tar.gz";
    hash = "sha256-p9yOoImV6qAwq9jjebq8fp2SngwNbZgVoJ6jmPDA0Lg=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
