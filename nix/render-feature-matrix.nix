# Rendered forms of the feature matrix: the SVG graphics the README embeds, plus a
# Markdown fallback. `nix build .#featureMatrix` produces all of them.

{ runCommand
, python3

, featureMatrixJson
}:

runCommand "feature-matrix" {
  nativeBuildInputs = [ python3 ];
  renderer = ../scripts/render-feature-matrix.py;
} ''
  mkdir -p $out

  cp ${featureMatrixJson} $out/feature-matrix.json

  python3 $renderer ${featureMatrixJson} --format svg --mode light -o $out/feature-matrix.svg
  python3 $renderer ${featureMatrixJson} --format svg --mode dark -o $out/feature-matrix-dark.svg
  python3 $renderer ${featureMatrixJson} --format markdown -o $out/feature-matrix.md
''
