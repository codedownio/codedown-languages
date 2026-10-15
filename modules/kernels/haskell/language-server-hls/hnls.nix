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
        sha256 = "sha256-BZ9u/MJnyPt0jmb3C5uvc5YQefFo832WfGXxCehUJrs=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-R/2HWyamUTQ+cJPgMQCLeLhv0P/vYfXwqHjUOSkYbXk=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-/zpOFaheQU2A2maoJBIsUvD98cITUK/iL8wDGUUQhOI=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-1p7A5iM/b3VSJtRSWWX/ZRJ/Bcugmfg7CitPyH7VXqI=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-z3eEnVtaMs9sZZhi0SxV9OwVPOrGL4DZFCtXobjl/Ho=";
      });
      "ghc94-aarch64-linux" = prebuilt "ghc94-aarch64-linux" (fetchzip {
        url = mkUrl "ghc94" "aarch64-linux";
        sha256 = "sha256-g7+reQF6K/WpnV/jcRrXEv08RFvz+h0u0XCz/Bo5xhw=";
      });
      "ghc96-aarch64-linux" = prebuilt "ghc96-aarch64-linux" (fetchzip {
        url = mkUrl "ghc96" "aarch64-linux";
        sha256 = "sha256-Rv6HnZ/OPlK01XZJNns2S8WDj9tQONpAgkW0cbGBhiA=";
      });
      "ghc98-aarch64-linux" = prebuilt "ghc98-aarch64-linux" (fetchzip {
        url = mkUrl "ghc98" "aarch64-linux";
        sha256 = "sha256-dGqFnDfYbBQH/PUcsXjm/1//mTkwkxnMOcVVATHtkKY=";
      });
      "ghc910-aarch64-linux" = prebuilt "ghc910-aarch64-linux" (fetchzip {
        url = mkUrl "ghc910" "aarch64-linux";
        sha256 = "sha256-HMPqgnigWz1XM6Y3eY3vOHsrtmCtb2dRGtClJU5Gx84=";
      });
      "ghc912-aarch64-linux" = prebuilt "ghc912-aarch64-linux" (fetchzip {
        url = mkUrl "ghc912" "aarch64-linux";
        sha256 = "sha256-ZBEZByN3/XWGSVkULlRL2+/EfmTy8VvqgZaTwdSj+Ik=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-7VTiBwYoxrv5U4IlESv/yMZOTjyHStFp5bGAaYpiJhw=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-1H5NUChh0XsnPNO9n8yZUgHknOkWII/P9UGL/8O3xUg=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-XEIK7+WhV7WokEzG+5K9KQ2m79KQ7p8TBAEL0xRCtTU=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-YoWrwyD4ONdAYL882eL/VTcMdKaAZgtfBf2HZKVSfgE=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-b3tvdiS+nfUSr1RDXBVS+CKpvuZgSP/QtTA3X+zyxT0=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-EiM+5GRvDwKquAHNlA8h3nenU/exWiH0VHUUBa7fnOA=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-YytAS/JxVDz+hdtxkU3AhJ3y2ffao3u2x/gxJkI4z6c=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-OWfSLXWxYftjNoarRfe3OOTaowb1Zci7bGtUoK2htKs=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-weKLxvHRmQr5As8BCwiD/hh6EtQY0Rb0TqF+i8xLqZk=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-NnRUiOGLEEp0YtmDjmc6RGwg2nt80kveQOlFik98/7Q=";
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
