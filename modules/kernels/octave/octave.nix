{ lib
, makeWrapper
, runCommand

, octaveComplete

, makeFontsConf
, carlito
, dejavu_fonts
, font-misc-misc
, freefont_ttf

, epstool
, fig2dev
, fontconfig
, ghostscript
, gnuplot
, graphicsmagick
, pstoedit
, texinfo
}:

let
  fontsConf = makeFontsConf {
    fontDirectories = [
      carlito dejavu_fonts
      freefont_ttf font-misc-misc
      # liberation_ttf_v1_binary
      # liberation_ttf_v2_binary
    ];
  };

  # Binaries octave needs at runtime
  # (For some reason these don't seem to be included by the default Octave derivation)
  octaveBinaries = [epstool fig2dev fontconfig ghostscript gnuplot graphicsmagick pstoedit texinfo];

in

runCommand "octave-with-binaries" {
  # version = octaveComplete.version;

  buildInputs = [makeWrapper octaveComplete] ++ octaveBinaries;

  propagatedBuildInputs = [octaveComplete] ++ octaveBinaries;
} ''
  mkdir -p $out/bin

  makeWrapper ${octaveComplete}/bin/octave $out/bin/octave \
    --set FONTCONFIG_FILE ${fontsConf} \
    ${lib.concatStringsSep " " (map (x: "--suffix PATH ':' ${x}/bin") octaveBinaries)}

  makeWrapper ${octaveComplete}/bin/octave-cli $out/bin/octave-cli \
    --set FONTCONFIG_FILE ${fontsConf} \
    ${lib.concatStringsSep " " (map (x: "--suffix PATH ':' ${x}/bin") octaveBinaries)}
''
