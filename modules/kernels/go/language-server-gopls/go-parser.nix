{ buildGoModule
, fetchFromGitHub
, lib
}:

buildGoModule rec {
  pname = "go-parser";
  version = import ./gnls-version.nix;

  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "go-notebook-language-server";
    rev = "v${version}";
<<<<<<< gnls
    hash = "sha256-bOh7LbPxs+olEGKrNnnoq6xMhoRRSeZCV6GgJ7CUPyk=";
=======
    hash = "sha256-SdN/v4psQRQ8O+3BosYgcZzBZSxNytVmw1A/lM2fQ/4=";
>>>>>>> main
  };

  sourceRoot = "${src.name}/go-parser";

  vendorHash = null;

  meta = with lib; {
    description = "Go notebook code parser using go/scanner";
    homepage = "https://github.com/codedownio/go-notebook-language-server";
    license = licenses.bsd3;
    platforms = platforms.unix;
    mainProgram = "go-parser";
  };
}
