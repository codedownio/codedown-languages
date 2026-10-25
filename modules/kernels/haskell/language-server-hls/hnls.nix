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
        sha256 = "sha256-L6TehMK581Wb2b5aZN18HPdhQliTUwkDusQ8CyJ8DS8=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-oJk0PpumY4KnJ+ydUtFaxZhLJyRsJxJTh3cAvJWjZZg=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-V6QE2o0eQb4ExfJlTtD+Rc5Irc6YOnKBDS8aCO9XWGI=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-7VfIrENEZfZ9sdYFP1+6ybaWEU1bEAnc6TAmGB6OI5E=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-rq2AKhC6Sq7vGh9MlML71q/3HbcJplCJMFM4YhhCOHY=";
      });
      "ghc914-x86_64-linux" = prebuilt "ghc914-x86_64-linux" (fetchzip {
        url = mkUrl "ghc914" "x86_64-linux";
        sha256 = "sha256-oD+XKKHcJ6eeV4NnM0KW9Rf6r/f8O94/h3uTAWDpFv0=";
      });
      "ghc94-aarch64-linux" = prebuilt "ghc94-aarch64-linux" (fetchzip {
        url = mkUrl "ghc94" "aarch64-linux";
        sha256 = "sha256-6CdsKOUk8yQr23NKUla2Zmnn9plo7koP0W9MBGcAhVI=";
      });
      "ghc96-aarch64-linux" = prebuilt "ghc96-aarch64-linux" (fetchzip {
        url = mkUrl "ghc96" "aarch64-linux";
        sha256 = "sha256-UYhF5Itcc5oCJTHwNFohphq4tq5bs3s/ciC5sOm+V3A=";
      });
      "ghc98-aarch64-linux" = prebuilt "ghc98-aarch64-linux" (fetchzip {
        url = mkUrl "ghc98" "aarch64-linux";
        sha256 = "sha256-tdPisMCOeYqiUE39gTlkNW0KeZnalH/r+9jWQgHEYy0=";
      });
      "ghc910-aarch64-linux" = prebuilt "ghc910-aarch64-linux" (fetchzip {
        url = mkUrl "ghc910" "aarch64-linux";
        sha256 = "sha256-hzeRiVY2Uh2NNfKvC5da06Da7T4mYzvU5WSYJTNOyI8=";
      });
      "ghc912-aarch64-linux" = prebuilt "ghc912-aarch64-linux" (fetchzip {
        url = mkUrl "ghc912" "aarch64-linux";
        sha256 = "sha256-ZdMPpHKs8I+Dtm+RsALD2IZXl/RboVn135QhSFFoj0E=";
      });
      "ghc914-aarch64-linux" = prebuilt "ghc914-aarch64-linux" (fetchzip {
        url = mkUrl "ghc914" "aarch64-linux";
        sha256 = "sha256-lINnwFT9EHdtyDNX5CONxOr3t9nXGRDKqQ7owZvhCHU=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-WBcL9gwAGEBJz3O83yyIYm7kZQKVJCgsStkPZNa0cDs=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-rQW/dbzPIRFCQ/QVmUUW74ouJQKbTaLDErXxw8z9+mg=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-k5x+9Q2GBtjomcWq1tNyMXitIFo0NTx9RLq3M14AjTw=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-IptKtRkgjcKPzycyfNmvmKuMhbgz+lHT2z4GwbvR52o=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-PomPet/aukKaIzAtpngTWxiZykhg9vjnRENg588nEKM=";
      });
      "ghc914-x86_64-darwin" = prebuilt "ghc914-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc914" "x86_64-darwin";
        sha256 = "sha256-6NUmUPTFhRShHumhog0aaS93W8q9XWZLwu6ZRrE4cuo=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-0WoF4mUwNOrNYFdApNSX0hNsfHOLyqOP02/1Z/bgUfQ=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-HCILHI18ZTpnec4kxoKKyctn5d8aGr3lh3mbaVRYHG4=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-aBnsn8yg3V3dy15QvxPM5e4rDzr8MzzFZCmld/1lwpA=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-iU+cBra/tf5WN/HAabIBHVCPvT/gWUY57KXNtIReQ8A=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-TxXbAcTkLkNIusGAlWXGDucS++uSsLv4rTsrJ/vTVtw=";
      });
      "ghc914-aarch64-darwin" = prebuilt "ghc914-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc914" "aarch64-darwin";
        sha256 = "sha256-L5P9sWGyhPZ9n+Iqn3bL2SxmFHCiskkpkZBQC+yREFI=";
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
