# JJava, the maintained continuation of IJava. The release zip is a shaded jar plus a
# launcher, so there's no Maven build and no mvnHash; pure Java, so one hash covers all
# platforms.
{ lib
, fetchzip
}:

let
  version = import ./jjava-version.nix;

in

fetchzip {
  name = "jjava-${version}";
  url = "https://github.com/dflib/jjava/releases/download/${version}/jjava-${version}-kernelspec.zip";
  stripRoot = false; # the zip has no top-level directory
  hash = "sha256-E0XHgyVtpeHLBUntTJ08mq5g9sGwhI7ilCyC3PwL5yY=";

  meta = {
    description = "Jupyter kernel for Java, built on JShell";
    homepage = "https://github.com/dflib/jjava";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
