{ fetchFromGitHub
, fetchurl
, fetchzip
, lib
, stdenv
, system

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
    mkUrl = ghc: system: "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-${ghc}-${system}.tar.gz";
  in
    {
      # HASHES_START
      "ghc92-x86_64-linux" = prebuilt "ghc92-x86_64-linux" (fetchzip {
        url = mkUrl "ghc92" "x86_64-linux";
        sha256 = "sha256-V2DBheM1Cnj1pGFy/O2Iw9q3/KZ4/bviz/SO+TaPW7U=";
      });
      "ghc94-x86_64-linux" = prebuilt "ghc94-x86_64-linux" (fetchzip {
        url = mkUrl "ghc94" "x86_64-linux";
        sha256 = "sha256-TK450kbql86TcxGb7CVmUqyJelzHm2XrgE9AZzE4bgc=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-UgyGWSCmDi7qIXpRc7b6FMYbnFhfxgKGepaPfiGsT88=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-j+9EP3UUHppolxDoH0LQGZ2O0UNJ/ijJk65S8rVscF4=";
      });
      "ghc92-x86_64-darwin" = prebuilt "ghc92-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc92" "x86_64-darwin";
        sha256 = "sha256-GUG58ln5u9Fiqdvt++lXDR4HveQOtSHH6EdRB6+a7Gk=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-hxMNYSBfcJkE+6hU45wZaRBcXjjylhuJ6SVVi5vCSyY=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-eCK1JohWfe2NdUXmL8SlaZhQWsyI1e2L7YuzozgATO4=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-qXhtQQb1tVNJABqTpyLUTho1zAJ9F6BCaMUJ8osVdkY=";
      });
      "ghc92-aarch64-darwin" = prebuilt "ghc92-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc92" "aarch64-darwin";
        sha256 = "sha256-3wvFTtwQez4OlHzTIL0Fo1Dk30llXKilYHunv2feLjk=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-ngWA7/MNrHuVpe0gnBdJOZYHJ3pDBvu0THAslKy5XBM=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-rbaczumfoC9bOeu4VU1b6yG50tdnnL+7LYw4GCXLgdY=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-7vM7WKqAoBp8WsZUQ/+SmfBbwHtU3B85V4S/9spJZzc=";
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

  desiredVersion = "ghc" + (builtins.replaceStrings ["."] [""] (lib.versions.majorMinor ghc.version)) + "-" + system;

in

if builtins.hasAttr desiredVersion versions
then builtins.getAttr desiredVersion versions
else throw ("Unsupported GHC version: " + ghc.version)
