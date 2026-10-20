# leanprover-community/repl: a line-oriented JSON frontend to Lean's elaborator. It's what
# the kernel drives to execute cells, and it's the reason this kernel is packageable at all --
# its lake-manifest.json lists zero packages, so `lake build` needs no network.
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

  # The Lean compiler emits C and hands it to a C compiler to build and link. With a host gcc
  # the 24 Lean targets all compile and only the final link fails, on undefined
  # `std::__detail::__wait_impl` (libstdc++ too old) and `free_sized` (glibc too old). Keeping
  # stdenv's cc the only compiler on PATH is what makes it link, so don't add a toolchain here.
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

    # The binary locates its own oleans from any cwd, but it execs `lean` for elaboration,
    # so the toolchain has to be on its PATH no matter who spawns it.
    makeWrapper "$out/bin/.repl-unwrapped" "$out/bin/repl" \
      --prefix PATH : ${lean4}/bin

    runHook postInstall
  '';

  # Upstream's test.sh diffs test/*.in through the built repl against checked-in expected
  # output, then cd's into test/Mathlib and does it again -- that second half fetches Mathlib
  # and can't run here, so drive the first half ourselves rather than calling test.sh.
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
