{ fetchFromGitHub
, stdenv

, gcc
, zeromq
, mitscheme
, openssl
, pkg-config
, which
}:

stdenv.mkDerivation rec {
  name = "mit_scheme_kernel";

  src = fetchFromGitHub {
    owner = "joeltg";
    repo = "mit-scheme-kernel";
    rev = "6de0a332d64960b47853a2ddcf8572a248fadad0";
    sha256 = "1vx8kia6qgcw432jrks1578qnl8g7py5m75z0fxskpzsbp2qhfys";
  };

  buildInputs = [ gcc zeromq mitscheme openssl pkg-config which ];

  propagatedBuildInputs = [ mitscheme openssl pkg-config ];

  patchPhase = ''
    sed -i "s|-undefined dynamic_lookup||g" makefile
    # For some reason, pkg-config isn't being found in the buildPhase even though we're
    # including pkg-config.
    sed -i "s|\`pkg-config --libs libzmq\`|-lzmq|g" makefile
  '';

  buildPhase = ''
    sed -i "s|AUXDIR=/usr/local/lib/mit-scheme-x86-64|AUXDIR=${mitscheme}/lib/mit-scheme-x86-64|g" makefile
    sed -i "s|SCHEME=/usr/local/bin/mit-scheme|SCHEME=${mitscheme}/bin/mit-scheme|g" makefile
    make
  '';

  installPhase = ''
    # Fix the scheme library path
    sed -i "s|/usr/local/share/jupyter/kernels/mit-scheme|$out/lib/src|g" src/load.scm
    # Fix the LD_LIBRARY_PATH
    sed -i "s|/usr/local/lib/mit-scheme-x86-64|${mitscheme}/lib/mit-scheme-x86-64:${zeromq}/lib|g" src/zmq.scm
    # Fix the path to openssl
    sed -i "s|openssl|${openssl}/bin/openssl|g" src/kernel/utils.scm

    mkdir -p $out/lib
    cp -rv ./* $out/lib

    mkdir -p $out/bin
    cat > $out/bin/mit_scheme_kernel <<EOF
    #!/bin/sh
    ${mitscheme}/bin/scheme --library ${mitscheme}/lib/mit-scheme-x86-64:$out/lib --silent --eval "(define pathname (working-directory-pathname))" --load $out/lib/src/load.scm -- \$1
    EOF
    chmod 755 $out/bin/mit_scheme_kernel
  '';

  doCheck = false;

  meta = {
    description = "Jupyter Kernel for MIT Scheme";
    homepage = https://github.com/joeltg/mit-scheme-kernel;
  };
}
