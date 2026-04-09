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
    hash = "sha256-DEl//m7jt4KSP5rd3bTEfN2Zp/A/zqi4hZU2tJbh4co=";
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
