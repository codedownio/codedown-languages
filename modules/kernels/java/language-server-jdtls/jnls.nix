{ fetchzip
, stdenv
}:

# Static binaries from GitHub releases. ./jnls-update.sh rewrites these and java-parser.nix's
# source hash together.
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.0.0/java-notebook-language-server-0.1.0.0-aarch64-linux.tar.gz";
    hash = "sha256-cWZyiZCAKu/cpfwD2DWkXcUkX8lcIqYWLgj0cuKqvRw=";
  };
  "x86_64-linux" = fetchzip {
    name = "java-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.0.0/java-notebook-language-server-0.1.0.0-x86_64-linux.tar.gz";
    hash = "sha256-xiOxhjPLdXcHDzauYQZh15hkgLfVjlrH72E/dcPXFXs=";
  };
  "x86_64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.0.0/java-notebook-language-server-0.1.0.0-x86_64-darwin.tar.gz";
    hash = "sha256-JzV7mKviDosRrDAQ2dc5xVH/mKMtk/NWQ/mRjUlMytQ=";
  };
  "aarch64-darwin" = fetchzip {
    name = "java-notebook-language-server-0.1.0.0";
    stripRoot = false;
    url = "https://github.com/codedownio/java-notebook-language-server/releases/download/v0.1.0.0/java-notebook-language-server-0.1.0.0-aarch64-darwin.tar.gz";
    hash = "sha256-6UuO8whigtjQRjY4clBshpnTnSG/+P59URuCP5RHU/Q=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
