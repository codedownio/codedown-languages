{ fetchFromGitHub
, gettext
, lib
, stdenv

, rPackages
, rWrapper
}:

let
  # DESCRIPTION's Imports, minus the ones that ship with R (parallel, tools, utils).
  languageServerDeps = with rPackages; [
    R6
    callr
    collections
    digest
    fs
    jsonlite
    lintr
    roxygen2
    stringi
    styler
    xml2
    xmlparsedata
  ];

  buildR = rWrapper.override {
    packages = languageServerDeps;
  };

in

stdenv.mkDerivation {
  name = "r-custom-languageserver";
  version = "0.3.20";

  # Our fork of upstream master, carrying one commit: shutdown answers with a null result
  # rather than `[]`, which clients that type the result strictly can't decode. That branch
  # is the one to send upstream as a PR; point this back at REditorSupport once it lands.
  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "languageserver";
    rev = "de6d8ba3b36f5da85f6398c53982c327c3fedacd";
    sha256 = "16y7i1mwk97sq4yyca52v1hpm55aamdyf6yzm90qalh18g8av44g";
  };

  configurePhase = ''
    runHook preConfigure
    export R_LIBS_SITE="$R_LIBS_SITE''${R_LIBS_SITE:+:}$out/library"
    runHook postConfigure
  '';

  buildInputs = [
    buildR
  ] ++ lib.optionals stdenv.isDarwin [
    gettext
  ];

  buildPhase = ''
    runHook preBuild
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/library
    R CMD INSTALL $installFlags --configure-args="$configureFlags" -l $out/library .
    runHook postInstall
  '';

  postFixup = ''
    if test -e $out/nix-support/propagated-build-inputs; then
    ln -s $out/nix-support/propagated-build-inputs $out/nix-support/propagated-user-env-packages
    fi
  '';

  meta = {
    description = "An implementation of the Language Server Protocol for R";
    homepage = "https://github.com/REditorSupport/languageserver";
  };

  passthru = {
    inherit languageServerDeps;
  };
}
