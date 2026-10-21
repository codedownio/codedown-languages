# leanprover-community/repl: JSON-over-stdio frontend to Lean's elaborator, which the
# kernel drives to run cells. Its lake-manifest.json is empty, so the build is offline.
{ lib
, stdenv
, fetchFromGitHub
, makeWrapper

, lean4
}:

let
  replVersion = import ./repl-version.nix;

in

stdenv.mkDerivation {
  pname = "lean4-repl";
  version = lean4.version;

  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "repl";
    rev = replVersion.rev;
    sha256 = replVersion.sha256;
  };

  nativeBuildInputs = [ lean4 makeWrapper ];

  # Lean emits C and links it with whatever cc is on PATH. A host gcc compiles every target
  # and then fails the final link on libstdc++/glibc symbols, so leave stdenv's cc alone.
  buildPhase = ''
    runHook preBuild

    # lake writes its build cache under HOME and fails outright if it isn't writable.
    export HOME="$TMPDIR"

    lake build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin" "$out/lib/lean"
    cp .lake/build/bin/repl "$out/bin/.repl-unwrapped"
    cp -r .lake/build/lib/lean/. "$out/lib/lean/"

    # Finds its own oleans from any cwd, but execs `lean`, so needs it on PATH.
    makeWrapper "$out/bin/.repl-unwrapped" "$out/bin/repl" \
      --prefix PATH : ${lean4}/bin

    runHook postInstall
  '';

  # Upstream's test.sh also runs a Mathlib half that needs the network, so drive the
  # test/*.in cases ourselves rather than calling it.
  doCheck = true;
  checkPhase = ''
    runHook preCheck

    # LC_COLLATE so `foo.in` sorts before `foo2.in`, as upstream's loop assumes.
    export LC_COLLATE=C

    for infile in test/*.in; do
      expected="''${infile%.in}.expected.out"
      [ -f "$expected" ] || continue
      echo "repl test: $(basename "$infile" .in)"
      .lake/build/bin/repl < "$infile" > actual.out 2>&1
      diff -u "$expected" actual.out
    done

    runHook postCheck
  '';

  meta = {
    description = "A JSON-over-stdio read-eval-print loop for Lean 4";
    homepage = "https://github.com/leanprover-community/repl";
    license = lib.licenses.asl20;
    mainProgram = "repl";
    platforms = lean4.meta.platforms;
  };
}
