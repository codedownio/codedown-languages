{ fetchFromGitHub
, gettext
, lib
, stdenv

, rPackages
, rWrapper
}:

let
  languageServerDeps = with rPackages; [
    R6
    callr
    collections
    desc
    fs
    jsonlite
    lintr
    repr
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
  version = "0.3.16";

  src = fetchFromGitHub {
    owner = "REditorSupport";
    repo = "languageserver";
    rev = "beed0f00563390b3e39a9bb47fc25bf852ec3734";
    sha256 = "0sifrmkv6hn15nppjg4wjvs3l8an3lsa4rdp3xi3f4w006kdgx5w";
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
