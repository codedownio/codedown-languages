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
        sha256 = "sha256-FYEe39WOqt5bBEEP/kFsRoKSCBKYq+BxjbcePEE3XKg=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-jj8ujG1ub3w9pbknW1/5Cg1inYInCnN5OThzw/rzSEE=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-PrycBr6W0g4vAv027g+IUH6iKL9hixcoIalb/pz6Wxc=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-gbjCDMLza8ZNZoYi1+bW5ZZn/R8i0kUa9VczVPAqbI8=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-/ydRL0V6J9McaH50zjZF7b6QwhplqDnwzPBep7mS530=";
      });
      "ghc94-aarch64-linux" = prebuilt "ghc94-aarch64-linux" (fetchzip {
        url = mkUrl "ghc94" "aarch64-linux";
        sha256 = "sha256-R8fPQlGosFUMXfUnIeQGzzKnCucw7gUIlRnqUe98ROg=";
      });
      "ghc96-aarch64-linux" = prebuilt "ghc96-aarch64-linux" (fetchzip {
        url = mkUrl "ghc96" "aarch64-linux";
        sha256 = "sha256-YX0TkKQBngkHWBrtdmsY1tEv/I59X9Ez9oiMCmY301I=";
      });
      "ghc98-aarch64-linux" = prebuilt "ghc98-aarch64-linux" (fetchzip {
        url = mkUrl "ghc98" "aarch64-linux";
        sha256 = "sha256-fB99HdtalB6UOd7ppE5sgnlvkGKi29WP+UlxbKZEgKo=";
      });
      "ghc910-aarch64-linux" = prebuilt "ghc910-aarch64-linux" (fetchzip {
        url = mkUrl "ghc910" "aarch64-linux";
        sha256 = "sha256-UzxWc9wOXxqY2sP+x8bsaVdRYz9Gelbtmurkh0X757Q=";
      });
      "ghc912-aarch64-linux" = prebuilt "ghc912-aarch64-linux" (fetchzip {
        url = mkUrl "ghc912" "aarch64-linux";
        sha256 = "sha256-Xg8Ee69mUuZXvgwx9NJDWhF+PJtIduiO2wwegfr+m7I=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-0kXe9pNpEt+n44jmQTj6eiBlGpmpKykWptzZBTE+nE4=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-CM1AUvb4EP9YHRRLyAyeVLfN4qWp/LrEn7WgzzTbRrU=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-oCv1IzCd6nz8FSOUoGVt0w/VBAVUN1wRmFs8lnQY9gM=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-Js+QAQP5Pdx+FI7r8Y5jfOqg6MHQdvBcIazFMo5tyrI=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-SP5q+WosEx+A6sFNvN5yKM75Pbgq7OPt8NNJUYqUJK8=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-K7J+Y07RaQWoxx3/iP797U2ZW/IXjbTkHC5R9dWofRI=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-wlnpQaPDiA6e8F81Pjg6wLj+AVTzNtFhbCFWXUmKO18=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-HPViPoe1wjmDlJCbnbv/V0dFQXqBlPYv7IjsT2yfjLQ=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-ibiYuoJZq++SKRaNAHFfZzNwMHbu7xRFp5eg0VaDBJU=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-K3spGnJt5J2Ibdg8thH7nQOPiUGoev0O+pBtGynmk2Y=";
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
