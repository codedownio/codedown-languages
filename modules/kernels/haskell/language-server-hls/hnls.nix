{ fetchzip
, lib
, stdenv

, ghc
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
    mkUrl = ghc: sys: "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-${ghc}-${sys}.tar.gz";
  in
    {
      # HASHES_START
      "ghc94-x86_64-linux" = prebuilt "ghc94-x86_64-linux" (fetchzip {
        url = mkUrl "ghc94" "x86_64-linux";
        sha256 = "sha256-3zVquOJt8QQo66CJ/B+Q88gijs/auKD9x0yydieWF64=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-73KzVZ8NeH8M6/hCZiMz/T/1F1uHQeJ3e6c8IWgC6d0=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-ZRgramlo4GRWSc7BpDVTCfPDmSrRUbtgfgoyFT0rKdc=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-CS0/GgpNPrEnXZ1Q8loX1Zb7JdGUb+X5PFnc+JAi7a4=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-lbJSVpQMRNlxCFZZPwsqsjvjmeyU4zZt4g5S+ch5osk=";
      });
      "ghc94-aarch64-linux" = prebuilt "ghc94-aarch64-linux" (fetchzip {
        url = mkUrl "ghc94" "aarch64-linux";
        sha256 = "sha256-giSriUmHUT2cSoPgEfgwvk3YmDkKpcYWfeq8i744K50=";
      });
      "ghc96-aarch64-linux" = prebuilt "ghc96-aarch64-linux" (fetchzip {
        url = mkUrl "ghc96" "aarch64-linux";
        sha256 = "sha256-ab/SH2hHmeCX9jez0S7lNPiEe7MsdLpEBt9iRCeKl7o=";
      });
      "ghc98-aarch64-linux" = prebuilt "ghc98-aarch64-linux" (fetchzip {
        url = mkUrl "ghc98" "aarch64-linux";
        sha256 = "sha256-HLe/KOlPz0vU8Mxvnq+Rc65kjb8BoO53p+5684m9DX8=";
      });
      "ghc910-aarch64-linux" = prebuilt "ghc910-aarch64-linux" (fetchzip {
        url = mkUrl "ghc910" "aarch64-linux";
        sha256 = "sha256-kznzG2p9jlambTic3RIabObECJDKIVGekhrCyrFpHRc=";
      });
      "ghc912-aarch64-linux" = prebuilt "ghc912-aarch64-linux" (fetchzip {
        url = mkUrl "ghc912" "aarch64-linux";
        sha256 = "sha256-CblcFylQH8073iDEZutPuz5XcLpPeV794QHIWS2HN0c=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-DrDo2MSr2L+HV4Gzir0hhxZ+pBUG6yAHLt59jUPsU+A=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-wCDjjjfOTHkzBViq3KWOoI3iarzRkS3YnlarnTUJlIc=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-j6MD1posZnvSVO7P/4T1Oh3rnJOYBbE6vX9sfjUf0P8=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-Xz+DsufPzNHUw5ZXro8ECamWaCU88equ9vzLFdXqYEg=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-vhLUjt/Q+KCx6htqT8YEvEyU04baZ/XywoRatGp/CqI=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-MMAG2mVDuK87NprsYBvyu+f93mV9bpctqBYYFANP+3A=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-EoJQVwkjw+P0GG4G8VgbJo+lqxJUUDvx2sogKYqeipQ=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-TH1eVeYR79twzFg9t0YmdD/c8Qybqov9cHP2/3o8/LQ=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-60LXRgLanf9eh54zkUJl2BHIRe3wGeoyjAO8Q3MZOF0=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-ABs2quWwLyy4Jjfjru0yapx6B2SXGNZHc4yuVJCUB5w=";
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

  desiredVersion = "ghc" + (builtins.replaceStrings ["."] [""] (lib.versions.majorMinor ghc.version)) + "-" + stdenv.hostPlatform.system;

in

if builtins.hasAttr desiredVersion versions
then builtins.getAttr desiredVersion versions
else throw ("Unsupported GHC version ${desiredVersion} (had: ${toString (builtins.attrNames versions)})")
