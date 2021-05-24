with import <nixpkgs> {};
with rWrapper;
with rPackages;

let
  languageserver_dependencies = [callr jsonlite lintr R6 repr stringr styler desc collections];

in

stdenv.mkDerivation {
  name = "r-custom-languageserver";

  inherit languageserver_dependencies;

  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "languageserver";
    rev = "d4035dba8ce3d1b42be6f3b1955457eac44d725d";
    sha256 = "0300cvva7b7lc1dqhvzhd8ghn9xhb5i0yf2v731il869zn5ig0lw";
  };

  configurePhase = ''
    runHook preConfigure
    export R_LIBS_SITE="$R_LIBS_SITE''${R_LIBS_SITE:+:}$out/library"
    runHook postConfigure
  '';

  buildInputs = [R] ++ languageserver_dependencies;
  propagatedBuildInputs = [R] ++ languageserver_dependencies;

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
}


  # writeText "language_servers.yaml" (lib.generators.toYAML {} [{
  #   name = "r";
  #   extensions = ["r"];
  #   attrs = ["r"];
  #   type = "stream";

  #   # Run the language server via an intermediary process, since it refuses to run as a child of PID 1
  #   # See https://github.com/REditorSupport/languageserver/issues/25
  #   args = ["${rWithPackages}/bin/R" "--slave" "-e" "languageserver::run()"];
  # }])
