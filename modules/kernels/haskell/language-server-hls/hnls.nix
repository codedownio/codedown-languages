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
        sha256 = "sha256-XXh+ayqM+6ymliTSOfzE6oXENBROl3sbMbIA4qmBoWs=";
      });
      "ghc94-x86_64-linux" = prebuilt "ghc94-x86_64-linux" (fetchzip {
        url = mkUrl "ghc94" "x86_64-linux";
        sha256 = "sha256-00v4AJERRdsBcy/g6N4+lP7zaZhmQIca+yAbvR9sU4Y=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-vNQ+SUAbV1tMR/LY54b0xgFONl/tJxkKL5vYaxrfQGY=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-j63G/Tw91hsomCsH87JPrLdub146UeglKuM311UPXnw=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-43IRoIwHmlCMy53J/XdBeRadjecXG7jisWzXkL3bFA8=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-R6vr9m3G8u+I8Gmhd13yZL2ncOjg2qtEn9LkvKafjMo=";
      });
      "ghc92-aarch64-linux" = prebuilt "ghc92-aarch64-linux" (fetchzip {
        url = mkUrl "ghc92" "aarch64-linux";
        sha256 = "sha256-qK4BErU8u7MflMIHG79RpfeRmQH2wa8e12MwBUhguis=";
      });
      "ghc94-aarch64-linux" = prebuilt "ghc94-aarch64-linux" (fetchzip {
        url = mkUrl "ghc94" "aarch64-linux";
        sha256 = "sha256-Z0Y1j15IoZ2rNrv5ldb4fwnV9kt8ECjzcLgLYLeoq74=";
      });
      "ghc96-aarch64-linux" = prebuilt "ghc96-aarch64-linux" (fetchzip {
        url = mkUrl "ghc96" "aarch64-linux";
        sha256 = "sha256-JumFeoV83YU2oNy06srso1w3bY8q32v7sY0en89jVmc=";
      });
      "ghc98-aarch64-linux" = prebuilt "ghc98-aarch64-linux" (fetchzip {
        url = mkUrl "ghc98" "aarch64-linux";
        sha256 = "sha256-f0ylzP0j2PbuXogMPwb/6BtbsyW3RAZLbBGqlpZ3tEk=";
      });
      "ghc910-aarch64-linux" = prebuilt "ghc910-aarch64-linux" (fetchzip {
        url = mkUrl "ghc910" "aarch64-linux";
        sha256 = "sha256-vRA/JWZ+0lmtuCci48PVmNrj9HR5AiMcGei9D/lMdZA=";
      });
      "ghc912-aarch64-linux" = prebuilt "ghc912-aarch64-linux" (fetchzip {
        url = mkUrl "ghc912" "aarch64-linux";
        sha256 = "sha256-ojSBD+qXJmfuPKiP8WW96HxEVY89Nq8AzaO3joCTfLw=";
      });
      "ghc92-x86_64-darwin" = prebuilt "ghc92-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc92" "x86_64-darwin";
        sha256 = "sha256-uIhrJQ0mVNYDay6L7mRIh/qQsaVn9SK1yjNU5R17dOg=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-+eDz0Se6+zPGgeFVkJrwk3jUkvVDCrN9I1r1sOG3C8A=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-9DDMSvrsT98P5EN7zKI5UxQeq6Bc23RUXL9MU4EuyRo=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-XnKq5MyJJxeZghD43SrKxu9i5AwNfh86VBHYhUXk/FU=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-5cbrN4yPrKu0x5jKamog44DMu12yxlfXo0zLzUnHP1Q=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-10fC4BYoeEj3WbPXwfU/g0WNEJqSOV3FMsp4uEEi+9M=";
      });
      "ghc92-aarch64-darwin" = prebuilt "ghc92-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc92" "aarch64-darwin";
        sha256 = "sha256-NCHLb1aEGl2XwMYUVCctnrBkWH/8Hz8/dK4jk3lWnqA=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-0VpmZeF4iwfG3I+FyBzo/gj7b25g0+NiRB5I6ZfwKwI=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-fwYGBzxfLUMA5C2KEno1AqT5FD+WN/WzvpT+lH4fpkY=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-F3khVAb9IdkpYISnw3/NI4jUB2E+7Jl/AOR+liN87hs=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-ERKdhlqBqhBessGq8k8ZdtgBPEXWBE5odPj4wzqehrI=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-v4jgQ83hj6AXUC4eTwAzIHZDqie07QLFlcHDCc7Wt7c=";
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
