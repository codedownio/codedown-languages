# Supplemental native-dep overrides for crates that nixpkgs' defaultCrateOverrides
# doesn't cover but that show up in common notebook environments. Same shape as
# defaultCrateOverrides (crate name -> attrs: { nativeBuildInputs; buildInputs; ... }).
# Add a one-liner here when the survey turns up a new uncovered -sys crate.
{ lib
, pkg-config
, expat
, fontconfig
, freetype
, llvmPackages
, opencv
, rdkafka
}:

{
  # plotters -> font-kit. (nixpkgs only has the unrelated servo-fontconfig-sys.)
  yeslogic-fontconfig-sys = attrs: {
    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ fontconfig freetype expat ];
  };

  # bindgen-based crates: clang-sys links libclang at build time, so the build
  # script binary needs it both as LIBCLANG_PATH and loadable at runtime (the
  # latter via the LD_LIBRARY_PATH baked from buildInputs in default.nix). This
  # unblocks the whole bindgen-via-clang-sys class, not just opencv.
  clang-sys = attrs: {
    buildInputs = [ llvmPackages.libclang ];
    LIBCLANG_PATH = "${lib.getLib llvmPackages.libclang}/lib";
  };

  # opencv (Rust bindings) parses the OpenCV headers with libclang and links
  # the system OpenCV libs found via pkg-config. clang-sys (above) supplies
  # libclang; opencv-binding-generator also shells out to the `clang` binary.
  opencv = attrs: {
    nativeBuildInputs = [ pkg-config llvmPackages.clang ];
    buildInputs = [ opencv llvmPackages.libclang ];
  };

  # rdkafka-sys defaults to compiling a *bundled* librdkafka, which fails
  # against our read-only vendored sources (its autotools build can't write
  # back into the Nix store). The working path is the system library via the
  # crate's `dynamic-linking` feature -- i.e. request rdkafka as
  # `{ name = "rdkafka"; features = ["dynamic-linking"]; }`. We provide the
  # library + pkg-config so that path links.
  rdkafka-sys = attrs: {
    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ rdkafka ];
  };
}
