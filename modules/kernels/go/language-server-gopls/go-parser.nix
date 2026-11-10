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
    # SOURCE_HASH: rewritten by ./gnls-update.sh, which looks for this comment.
    hash = "sha256-AWsTD5h5xEXJC4eZIN2TyQKsLX++zg4/TA/RM6XpEcA=";
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
