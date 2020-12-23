let nixpkgs = import (import ../../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;

let
  tiff = fetchurl {
    url = https://gr-framework.org/downloads/3rdparty/tiff-4.0.10.tar.gz;
    sha256 = "1r4np635gr6zlc0bic38dzvxia6iqzcrary4n1ylarzpr8fd2lic";
  };

  libogg = fetchurl {
    url = https://gr-framework.org/downloads/3rdparty/libogg-1.3.2.tar.gz;
    sha256 = "14j6clxdf48hshybwz5ic5r0wqvhgq9z91v2na635byp253y77p1";
  };

  libtheora = fetchurl {
    url = https://gr-framework.org/downloads/3rdparty/libtheora-1.1.1.tar.gz;
    sha256 = "0swiaj8987n995rc7hw0asvpwhhzpjiws8kr3s6r44bqqib2k5a0";
  };

  libvpx = stdenv.mkDerivation {
    name = "libvpx.tar.bz2";
    version = "1.4.0";

    src = fetchTarball {
      url = https://gr-framework.org/downloads/3rdparty/libvpx-1.4.0.tar.bz2;
      sha256 = "1y8cf2q5ij8z8ab5j36m18rbs62aah6sw6shzbs3jr70ja0z6n8s";
    };

    buildInputs = [perl];

    dontConfigure = "true";

    buildPhase = ''
      patchShebangs --build .
    '';

    installPhase = ''
      tar -cjSf $out . --transform 's,^,libvpx-1.4.0/,';
    '';
  };

  libopenh264 = fetchurl {
    url = https://gr-framework.org/downloads/3rdparty/libopenh264-2.0.0.tar.gz;
    sha256 = "1zb18npvy9nc4vbrqbs1kpsx52w2mlqnvfya3v8n0xa8rj05zhvk";
  };

  ffmpeg = fetchurl {
    url = https://gr-framework.org/downloads/3rdparty/ffmpeg-4.2.1.tar.gz;
    sha256 = "1kslz0z8zacgqkyp31mqh3lim7ykd811wighzi3dcsgad8dlfx5s";
  };

in

stdenv.mkDerivation {
  pname = "gr";
  version = "0.53.0";

  src = fetchgit {
    url = "https://github.com/sciapp/gr.git";
    rev = "v0.53.0";
    sha256 = "1a3zakisf77ip75z7n5wayyfx0k67sqgklg4hqdkzv7bw43rrmmj";
  };

  # cmakeFlags = [
  #   "-DCMAKE_INSTALL_PREFIX=$out"
  # ];

  # See https://gr-framework.org/building.html
  buildInputs = [
    curl # TODO: remove, useful to see what it's trying to download

    # misc
    perl

    # X11 (required)
    xorg.libX11
    xorg.libXft
    xorg.libXt

    # LaTeX support
    # texlive.dvipng
    # texlive-latex3 # TODO


  ];

  configurePhase = ''
    sed -i 's|$(DOWNLOAD_CMD) https://gr-framework.org/downloads/3rdparty/tiff-$(VERSION).tar.gz|cp ${tiff} ./tiff-4.0.10.tar.gz|g' 3rdparty/tiff/Makefile
    sed -i 's|$(DOWNLOAD_CMD) https://gr-framework.org/downloads/3rdparty/libogg-$(VERSION).tar.gz|cp ${libogg} ./libogg-1.3.2.tar.gz|g' 3rdparty/ogg/Makefile
    sed -i 's|$(DOWNLOAD_CMD) https://gr-framework.org/downloads/3rdparty/libtheora-$(VERSION).tar.gz|cp ${libtheora} ./libtheora-1.1.1.tar.gz|g' 3rdparty/theora/Makefile
    sed -i 's|$(DOWNLOAD_CMD) https://gr-framework.org/downloads/3rdparty/libvpx-$(VERSION).tar.bz2|cp ${libvpx} ./libvpx-1.4.0.tar.bz2|g' 3rdparty/vpx/Makefile
    sed -i 's|$(DOWNLOAD_CMD) https://gr-framework.org/downloads/3rdparty/libopenh264-$(VERSION).tar.gz|cp ${libopenh264} ./libopenh264-2.0.0.tar.gz|g' 3rdparty/openh264/Makefile
    sed -i 's|$(DOWNLOAD_CMD) https://gr-framework.org/downloads/3rdparty/ffmpeg-$(VERSION).tar.gz|cp ${ffmpeg} ./ffmpeg-4.2.1.tar.gz|g' 3rdparty/ffmpeg/Makefile

    patchShebangs --build .
  '';

  buildPhase = ''
    ls -lh /bin
    mkdir -p $out
    # make GRDIR="$out"
    make self
  '';
}
