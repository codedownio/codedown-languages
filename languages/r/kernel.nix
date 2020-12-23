let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with rWrapper;
with rPackages;

# Inspired by R generic-builder.nix
# TODO: try to directly call generic-builder
let
  irkernel_dependencies = [gettext repr evaluate IRdisplay pbdZMQ crayon jsonlite uuid digest];

in

stdenv.mkDerivation {
  name = "r-irkernel";

  inherit irkernel_dependencies;

  src = fetchFromGitHub {
    owner = "IRkernel";
    repo = "IRkernel";
    rev = "97c492b2e55b9fa556019d4b8625318614c9db87";
    sha256 = "0vwa2hjm40cmxzrmbvnz2w2c64zp8acdv1p2jvg7mhxvr0dvafff";
  };

  configurePhase = ''
    runHook preConfigure
    export R_LIBS_SITE="$R_LIBS_SITE''${R_LIBS_SITE:+:}$out/library"
    runHook postConfigure
  '';

  buildInputs = [R] ++ irkernel_dependencies;

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
