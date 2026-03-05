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
        cp -r ./. $out
      '';
    };
    mkUrl = ghc: system: "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-${ghc}-${system}.tar.gz";
  in
    {
      # HASHES_START
      "ghc92-x86_64-linux" = prebuilt "ghc92-x86_64-linux" (fetchzip {
        url = mkUrl "ghc92" "x86_64-linux";
        sha256 = "sha256-A4cXUY8b4TMuJiOkm6xw4gGZVKgTmuwgB+3OxKtGaAE=";
      });
      "ghc94-x86_64-linux" = prebuilt "ghc94-x86_64-linux" (fetchzip {
        url = mkUrl "ghc94" "x86_64-linux";
        sha256 = "sha256-GCD6jLYKzTRBQq/Roaik2lUpJ+3rjI922yNiFnQVW08=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-IiH6OEfYTgQ9Ct3LNZnkUCrbp9g5ciDnPU136nrM55I=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-D9miOCgTSbXcT3bqec4A5qvmhOZYPkh3RB2Q3nitXgI=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-V/aMbdcMEAxaMibJD7vJ5GvrL4WHW+w3diQmt3ExqoM=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-g+DhQpqAgrrLfZqT3fhA6V2e+b4moL6TVGc1TFiMO6M=";
      });
      "ghc92-aarch64-linux" = prebuilt "ghc92-aarch64-linux" (fetchzip {
        url = mkUrl "ghc92" "aarch64-linux";
        sha256 = "sha256-qtVM8f/5J7igmat6KaZffN5vg1S9D05GWqeklVJJCVU=";
      });
      "ghc94-aarch64-linux" = prebuilt "ghc94-aarch64-linux" (fetchzip {
        url = mkUrl "ghc94" "aarch64-linux";
        sha256 = "sha256-IcjqNBHm+CG0vsCus2Ywmw7BvWv610RIpqE11+ivhzY=";
      });
      "ghc96-aarch64-linux" = prebuilt "ghc96-aarch64-linux" (fetchzip {
        url = mkUrl "ghc96" "aarch64-linux";
        sha256 = "sha256-YxiD9pc1HgOHEZj7V2PDoAQw6IZVVxMxHlFvIMrNO7Y=";
      });
      "ghc98-aarch64-linux" = prebuilt "ghc98-aarch64-linux" (fetchzip {
        url = mkUrl "ghc98" "aarch64-linux";
        sha256 = "sha256-lx9tAvLL1EXUbYvboYJ2py7YSgZR2+/weL25+nlFJX4=";
      });
      "ghc910-aarch64-linux" = prebuilt "ghc910-aarch64-linux" (fetchzip {
        url = mkUrl "ghc910" "aarch64-linux";
        sha256 = "sha256-G8Viw8F1f6cLDYJzBZbXJ9A1fRMPolLrCC+abDbZQHc=";
      });
      "ghc912-aarch64-linux" = prebuilt "ghc912-aarch64-linux" (fetchzip {
        url = mkUrl "ghc912" "aarch64-linux";
        sha256 = "sha256-dHVazvXAV/RTXSguNJuCK0OpW++WZrcBhCPtzfgzsSQ=";
      });
      "ghc92-x86_64-darwin" = prebuilt "ghc92-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc92" "x86_64-darwin";
        sha256 = "sha256-99l/XIpx17MmDYQFKh3WYemvDHM3Xh3DgavkgYamJy0=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-pCgd/5b1uqhMcBpnqqAptWlVrBkJvzthGacx2G7B+EU=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-ORJBYYkVkI0u2DBiucfTJ4/NWz/4lX64YBffVlunxl8=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-hhJ1//2jl2tiKRaFcflCY9ugxx8BCsSjVDAnDocsV3Q=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-9TTBdOVD5ffnan4kbTl72qjBuLQuFc0UjAlFvK2mEoE=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-adN+yfSmoOuJdHpXDoRxA+zmGqRIRndTo1PN2puOQU4=";
      });
      "ghc92-aarch64-darwin" = prebuilt "ghc92-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc92" "aarch64-darwin";
        sha256 = "sha256-Pe5Mply7T3oP6HJO/9cHkF9b41MfNcZReQuKgyqzuBQ=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-sdzZWDy24vbfzP3MqcLgw3J93HqRwekIDlid2dPNkMU=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-QSW0V7Jz7O1LK9xTQbWJCAe5/LYondixQ7MF6usN0+8=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-cnSKghCdLOdGiYCC94nt0+hKqaA69f8bhq4bfzwQxR8=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-1qNP2v2nmVquuEfqkVLsJZgkx/lIgZWplf27F1UXqgI=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-3mARmdWOr+4aRthT7LUAaDwSaCxCUEWfkwnced1kBg4=";
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
else throw ("Unsupported GHC version ${desiredVersion} (had: ${toString (builtins.attrNames versions)})")
