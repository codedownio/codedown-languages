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
        sha256 = "sha256-HhzLHmt5mhxOugV0y+8otHyMqgOB5WKeWdkkaKaUQXY=";
      });
      "ghc94-x86_64-linux" = prebuilt "ghc94-x86_64-linux" (fetchzip {
        url = mkUrl "ghc94" "x86_64-linux";
        sha256 = "sha256-ZRV8h3+C8jE20IUzSlW5WHoT+SJ/8yyQKWdihFCekl0=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-AUePZEEI+O7RPN2TTx0VZ0ao+IDfwY5ZAGh4juMjKzI=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-TE8BzBgKwLfX2tsjDXPuYj0quk/qRQTjb8gL/AGXAHI=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-FqQsQKiC7B/kxEZmplW0NrEcuE1frERo36WPlb9MgWA=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-RFWCniZ1zFrsesuEsTmvSVaIRak6atoLTIlxi6/fb5s=";
      });
      "ghc92-x86_64-darwin" = prebuilt "ghc92-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc92" "x86_64-darwin";
        sha256 = "sha256-VPHhZXSwfZiOhBX+iUABXp1nupInYERwB5XwKDw66so=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-newFtY/um66lYYdQF353TQnscraYIWekGRbA2QkVNiA=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-vNWVLoWtgrJd8GCVcw9U8PwcA9xKOWD3xmP9n8dYgn8=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-8HvGp1fq6YVUNlkCxWRcI+XygmkEjWGkWju9javwZnA=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-qVjydrJtukGF+PpkwoiS1aa1TTRpi5cO33yQYnjMEOs=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-WLy4puBOPssSj5PJhxHWenvaJJPF+Daqrj6g4XSA154=";
      });
      "ghc92-aarch64-darwin" = prebuilt "ghc92-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc92" "aarch64-darwin";
        sha256 = "sha256-JmSfec2j+bvu2+8mAd+doALsNPq8Aei6l1U5/fNV5XU=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-R8hOIYUIaDiqQqFleUyAR+Dlw4PuRMmfG7Sshxu8tMs=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-D+AqJYZ/IzxPGZjYXHl4Gq8mZIoOtQn3gJ6KM/rHEtw=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-zrLY/hqrIXrZeazylFTFO6dj5Lt+T9o++g5R2uuSk4s=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-Q7+uWdmpg5X0X7dDfMo8Ga32dukDl5kb+vjZ8QT6//E=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-5YsSOBkbqCuzbfWyUc5OnwmvxzAwOwCr960s0W7eVTc=";
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
