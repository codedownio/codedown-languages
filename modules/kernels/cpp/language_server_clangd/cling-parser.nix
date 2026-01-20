{ cling
, cmake
, fetchFromGitHub
, lib
, ncurses
, stdenv
, makeWrapper
, zlib
}:

let
  unwrapped = stdenv.mkDerivation rec {
    pname = "cling-parser";
    version = import ./cnls-version.nix;

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "cpp-notebook-language-server";
      rev = "v${version}";
      hash = "sha256-KGCj8pH38sFIgYEBC7bRycav1+rY8nidzDwC1qM6h5c=";
    };

    sourceRoot = "${src.name}/cling-parser";

    nativeBuildInputs = [ cmake ];

    buildInputs = [ cling.unwrapped zlib ncurses ];

    cmakeFlags = [
      "-DLLVM_CONFIG=${cling.unwrapped}/bin/llvm-config"
    ];

    meta = with lib; {
      description = "Minimal Cling parser for cpp-notebook-language-server";
      homepage = "https://github.com/codedownio/cpp-notebook-language-server";
      license = licenses.bsd3;
      platforms = platforms.all;
    };
  };

in

unwrapped.overrideAttrs (oldAttrs: {
  nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ makeWrapper ];

  # cling-parser needs a collection of flags to start up properly, so wrap it by default.
  # We'll provide the unwrapped version as a passthru
  flags = cling.flags ++ [
    "-resource-dir"
    "${cling.unwrapped}"
    "-L"
    "${cling.unwrapped}/lib"
    "-l"
    "${cling.unwrapped}/lib/cling.so"
  ];

  fixupPhase = ''
    runHook preFixup

    wrapProgram $out/bin/cling-parser \
      --argv0 $out/bin/.cling-parser-wrapped \
      --add-flags "$flags"

    runHook postFixup
  '';

  passthru = (oldAttrs.passthru or { }) // {
    inherit unwrapped;
  };
})
