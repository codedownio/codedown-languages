with import <nixpkgs> {};
with python3Packages;

let
  octaveComplete = octave.override {
    qscintilla = null;
    overridePlatforms = ["x86_64-linux" "x86_64-darwin"];
    gnuplot = gnuplot;
    ghostscript = ghostscript;
    graphicsmagick = graphicsmagick;
    python = python;
  };

  fontsConf = makeFontsConf {
    fontDirectories = [
      carlito dejavu_fonts
      freefont_ttf xorg.fontmiscmisc
      # liberation_ttf_v1_binary
      # liberation_ttf_v2_binary
    ];
  };

  # Binaries octave needs at runtime
  # (For some reason these don't seem to be included by the default Octave derivation)
  octaveBinaries = [epstool fig2dev pstoedit fontconfig ghostscript gnuplot graphicsmagick texinfo];

  in stdenv.mkDerivation {
    name = "octave-with-binaries";

    unpackPhase = "true";

    buildInputs = [makeWrapper octaveComplete] ++ octaveBinaries;
    propagatedBuildInputs = [octaveComplete] ++ octaveBinaries;

    buildPhase = ''
      mkdir -p $out/bin
      echo "fonts conf: $fontsConf"
      makeWrapper ${octaveComplete}/bin/octave $out/bin/octave --set FONTCONFIG_FILE ${fontsConf} ${lib.concatStringsSep " " (map (x: "--suffix PATH ':' ${x}/bin") octaveBinaries)}
      makeWrapper ${octaveComplete}/bin/octave-cli $out/bin/octave-cli --set FONTCONFIG_FILE ${fontsConf} ${lib.concatStringsSep " " (map (x: "--suffix PATH ':' ${x}/bin") octaveBinaries)}
    '';

    installPhase = "true";
  }
