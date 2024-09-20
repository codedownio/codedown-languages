{ fetchFromGitHub
, fetchurl
, fetchzip
, lib
, stdenv

, ghc
, snapshot
}:

let
  githubVersions = let
    version = import ./hnls-version.nix;
    prebuilt = ghcName: src: stdenv.mkDerivation {
      pname = "haskell-notebook-language-server-" + ghcName;
      inherit version;

      inherit src;

      installPhase = ''
        binary=$(find . -executable -type f | head -n 1)
        mkdir -p $out/bin
        cp "$binary" $out/bin/haskell-notebook-language-server
      '';
    };
    mkUrl = ghc: "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-${ghc}-x86_64-linux.tar.gz";
  in
    {
      # HASHES_START
      "ghc810" = prebuilt "ghc810" (fetchzip {
        url = mkUrl "ghc810";
        sha256 = "sha256-MZkn7CsQUfOqz0Zs4j5Kz3fBnTvDVKnL5WDjjSfuZLQ=";
      });
      "ghc90" = prebuilt "ghc90" (fetchzip {
        url = mkUrl "ghc90";
        sha256 = "sha256-YPUKNpdMtTuPthjWO/rbX880XqyvyaD++9Iy1j0L6gg=";
      });
      "ghc92" = prebuilt "ghc92" (fetchzip {
        url = mkUrl "ghc92";
        sha256 = "sha256-A8jGJ+0uE0Y4plAWsuexANZvvEF6IqqjWEeIR14EcAE=";
      });
      "ghc94" = prebuilt "ghc94" (fetchzip {
        url = mkUrl "ghc94";
        sha256 = "sha256-mVjZoPy/v6UbcbGgh5DH3N7F8ZdzL1EwmC8sqT6fPbA=";
      });
      "ghc96" = prebuilt "ghc96" (fetchzip {
        url = mkUrl "ghc96";
        sha256 = "sha256-WJRfnCQHWjjqZk54Awws881m7Zozw5L4irIukdE+z/M=";
      });
      "ghc98" = prebuilt "ghc98" (fetchzip {
        url = mkUrl "ghc98";
        sha256 = "sha256-gkxQG6xcitPP9s0Fdz2aA8qYIoyGVl6Wp+lBIuxZz7k=";
      });
      # HASHES_END
    };
  versions = githubVersions;

  # localFlakeVersions = let
  #   flakePackages = (builtins.getFlake "/home/tom/tools/haskell-notebook-language-server").packages.x86_64-linux;
  #   in {
  #     ghc8107 = flakePackages.ghc8107-static;
  #     ghc90 = flakePackages.ghc90-static;
  #     ghc92 = flakePackages.ghc92-static;
  #     ghc94 = flakePackages.ghc94-static;
  #     ghc96 = flakePackages.ghc96-static;
  #     ghc98 = flakePackages.ghc98-static;
  #   };
  # versions = localFlakeVersions;

  desiredVersion = "ghc" + (builtins.replaceStrings ["."] [""] (lib.versions.majorMinor ghc.version));

in

if builtins.hasAttr desiredVersion versions
then builtins.getAttr desiredVersion versions
else throw ("Unsupported GHC version: " + ghc.version)
