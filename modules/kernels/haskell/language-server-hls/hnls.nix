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
        sha256 = "sha256-+Gn/52j/64HGWTgu4fzdkHrrWnb1ddT8nEbGLEBVBGA=";
      });
      "ghc94-x86_64-linux" = prebuilt "ghc94-x86_64-linux" (fetchzip {
        url = mkUrl "ghc94" "x86_64-linux";
        sha256 = "sha256-cCi0DJpDbWaSgP5FzC0CmpWJNfTFyXi2uah7x/P7YSU=";
      });
      "ghc96-x86_64-linux" = prebuilt "ghc96-x86_64-linux" (fetchzip {
        url = mkUrl "ghc96" "x86_64-linux";
        sha256 = "sha256-uU3xs/HQZ1rbinG3Fz1fEbKaNBqsTcfF2zzQdmCO9zw=";
      });
      "ghc98-x86_64-linux" = prebuilt "ghc98-x86_64-linux" (fetchzip {
        url = mkUrl "ghc98" "x86_64-linux";
        sha256 = "sha256-tzIsPCfmrU63PTXzwxfhZgeM11tcsZ8j59+3QuFWfUM=";
      });
      "ghc910-x86_64-linux" = prebuilt "ghc910-x86_64-linux" (fetchzip {
        url = mkUrl "ghc910" "x86_64-linux";
        sha256 = "sha256-cglwjXyWUXWwJ7Oexvud+5m40/lopSfdT9UklZwtJkc=";
      });
      "ghc912-x86_64-linux" = prebuilt "ghc912-x86_64-linux" (fetchzip {
        url = mkUrl "ghc912" "x86_64-linux";
        sha256 = "sha256-Q8U5/setJmbxxjZ6zpytVKMxeeipvIV5YRc/I7xWbVk=";
      });
      "ghc92-aarch64-linux" = prebuilt "ghc92-aarch64-linux" (fetchzip {
        url = mkUrl "ghc92" "aarch64-linux";
        sha256 = "sha256-kIRQCzq3PEq9qTIG91Fj1Wr2a73SYFE+atX+IIdYqDE=";
      });
      "ghc94-aarch64-linux" = prebuilt "ghc94-aarch64-linux" (fetchzip {
        url = mkUrl "ghc94" "aarch64-linux";
        sha256 = "sha256-usK6oJnHjmw45JFWSLIZlJO18+Z3N2g20tpgIVZm2+o=";
      });
      "ghc96-aarch64-linux" = prebuilt "ghc96-aarch64-linux" (fetchzip {
        url = mkUrl "ghc96" "aarch64-linux";
        sha256 = "sha256-upwK/b4bUtX7I1/IuyZ1SN/uYyC+Enxq6ogyfHqmoK4=";
      });
      "ghc98-aarch64-linux" = prebuilt "ghc98-aarch64-linux" (fetchzip {
        url = mkUrl "ghc98" "aarch64-linux";
        sha256 = "sha256-20fQo/cDreI1kVEYchXubyOuQXlEIYmmjgzJcH5tN5w=";
      });
      "ghc910-aarch64-linux" = prebuilt "ghc910-aarch64-linux" (fetchzip {
        url = mkUrl "ghc910" "aarch64-linux";
        sha256 = "sha256-x+g8yoFgZo+MzxeYxrQ1ogL1B1AxzlYxjnAxciM/J+k=";
      });
      "ghc912-aarch64-linux" = prebuilt "ghc912-aarch64-linux" (fetchzip {
        url = mkUrl "ghc912" "aarch64-linux";
        sha256 = "sha256-s2nBhNmdBTWvsQlNu/LTCpPFYLV6qP9WJ/ZF4OFpXCY=";
      });
      "ghc92-x86_64-darwin" = prebuilt "ghc92-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc92" "x86_64-darwin";
        sha256 = "sha256-E9Y1VP0YjdUXeQNtcpHHHBth4mlpveV3OB1h6N3Aml4=";
      });
      "ghc94-x86_64-darwin" = prebuilt "ghc94-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc94" "x86_64-darwin";
        sha256 = "sha256-mBdLxm5garZKMu55GTE93NSbSkf8hRKImfY3GYt2PDI=";
      });
      "ghc96-x86_64-darwin" = prebuilt "ghc96-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc96" "x86_64-darwin";
        sha256 = "sha256-g5u+GnjzHnle02+1+CSvxuwe4rrmAp4Qfj/I5KO6EcQ=";
      });
      "ghc98-x86_64-darwin" = prebuilt "ghc98-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc98" "x86_64-darwin";
        sha256 = "sha256-Lah9M4jaH1FXbhuu1+SUZs0ghadqgym7mOIkGyuTEFo=";
      });
      "ghc910-x86_64-darwin" = prebuilt "ghc910-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc910" "x86_64-darwin";
        sha256 = "sha256-Yesy8NTp00e7KVZgxIbFdnSfY+vDV/QX0gFkzh87nuk=";
      });
      "ghc912-x86_64-darwin" = prebuilt "ghc912-x86_64-darwin" (fetchzip {
        url = mkUrl "ghc912" "x86_64-darwin";
        sha256 = "sha256-uqrTJXc3rleXaUg56nUyOdPMoU0jeZoYWAHItJvgZOI=";
      });
      "ghc92-aarch64-darwin" = prebuilt "ghc92-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc92" "aarch64-darwin";
        sha256 = "sha256-RBx+l4rOqqlO3vLkhMAI+txuXVLFr3e/uvFeG2EvzNo=";
      });
      "ghc94-aarch64-darwin" = prebuilt "ghc94-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc94" "aarch64-darwin";
        sha256 = "sha256-Y6K4h06QDwOxC99H2nF1J2cVcDqpvaKzbue2l+H2/J4=";
      });
      "ghc96-aarch64-darwin" = prebuilt "ghc96-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc96" "aarch64-darwin";
        sha256 = "sha256-fRd6vgWjNKQ5n6ePcEtIqWlaq0VNmOClRtRVJCEqSY8=";
      });
      "ghc98-aarch64-darwin" = prebuilt "ghc98-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc98" "aarch64-darwin";
        sha256 = "sha256-/ESUcaepDdMFzFdvvJ3o0UvJ6Si9+oJGm2SWQu5IYTk=";
      });
      "ghc910-aarch64-darwin" = prebuilt "ghc910-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc910" "aarch64-darwin";
        sha256 = "sha256-Z5flMGi2X+ec+3I1gcsNjUDWjP6xAgPq9duTQ2C/PxQ=";
      });
      "ghc912-aarch64-darwin" = prebuilt "ghc912-aarch64-darwin" (fetchzip {
        url = mkUrl "ghc912" "aarch64-darwin";
        sha256 = "sha256-fkIdXiIEvHHb639QuzjQC+2B4kHsEXNWeVCe4eZThoU=";
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
