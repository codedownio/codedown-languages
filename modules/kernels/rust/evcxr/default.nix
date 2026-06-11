{ callPackage
, lib
, libiconv
, makeWrapper
, python3
, runCommand
, stdenv

, CoreServices
, Security

, cargo
, defaultCrateOverrides
, evcxr
, rustPlatform
, rustc

, pkg-config
, expat
, fontconfig
, freetype
, llvmPackages
, opencv
, rdkafka
}:

let
  withPackages = callPackage ./withPackages.nix {
    inherit cargo rustPlatform;
  };

  # Supplemental native-dep overrides for crates that nixpkgs' defaultCrateOverrides
  # doesn't cover but that show up in common notebook environments. Same shape as
  # defaultCrateOverrides (crate name -> attrs: { nativeBuildInputs; buildInputs; }).
  # Add a one-liner here when the survey turns up a new uncovered -sys crate.
  extraCrateOverrides = {
    # plotters -> font-kit. (nixpkgs only has the unrelated servo-fontconfig-sys.)
    yeslogic-fontconfig-sys = attrs: {
      nativeBuildInputs = [ pkg-config ];
      buildInputs = [ fontconfig freetype expat ];
    };

    # bindgen-based crates: clang-sys links libclang at build time, so the build
    # script binary needs it both as LIBCLANG_PATH and loadable at runtime (the
    # latter via the LD_LIBRARY_PATH baked from buildInputs below). This unblocks
    # the whole bindgen-via-clang-sys class, not just opencv.
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
  };

  crateOverrides = defaultCrateOverrides // extraCrateOverrides;

  # Discover the native build inputs needed to compile a given package set.
  # evcxr invokes cargo *outside* any Nix build, so crates whose build scripts
  # link system libraries (openssl-sys, libsqlite3-sys, the fontconfig/freetype
  # crates pulled in by plotters, ...) would otherwise fail for lack of
  # pkg-config and the libraries themselves. We reuse nixpkgs'
  # defaultCrateOverrides: for every crate in the *resolved* dependency tree
  # that has an override, collect its nativeBuildInputs (tools like pkg-config /
  # cmake / protobuf) and buildInputs (libraries), and bake them into the
  # wrapper below.
  overrideInputs = packages:
    let
      crateNames = import "${withPackages.cargoNix packages}/crate-names.nix";
      present = builtins.intersectAttrs (lib.genAttrs crateNames (_: null)) crateOverrides;
      # The override values are functions of the crate's attrs; we only read the
      # build inputs and a few env vars, so a stub attrs value suffices (other
      # fields are never forced thanks to laziness + the denylist short-circuit).
      applied = lib.mapAttrsToList (_: override: override { version = "0"; }) present;

      # Attributes that are build inputs or buildRustCrate-only hooks, never
      # runtime env vars. Everything else that is a scalar (e.g. LIBCLANG_PATH,
      # LIBGIT2_SYS_USE_PKG_CONFIG) is propagated to the wrapper as an env var so
      # build scripts that read it at evcxr runtime behave like a Nix build.
      nonEnvAttrs = [
        "nativeBuildInputs" "buildInputs" "propagatedBuildInputs"
        "prePatch" "postPatch" "patchPhase" "preConfigure" "preBuild"
        "crateBin" "extraLinkFlags" "features"
      ];
      envOf = o: lib.mapAttrs
        (_: v: if builtins.isBool v then (if v then "1" else "") else toString v)
        (lib.filterAttrs (n: v:
          !(builtins.elem n nonEnvAttrs)
          && (builtins.isString v || builtins.isBool v || builtins.isInt v)) o);
    in {
      nativeBuildInputs = lib.concatMap (o: o.nativeBuildInputs or []) applied;
      buildInputs = lib.concatMap (o: o.buildInputs or []) applied;
      env = lib.foldl' (acc: o: acc // (envOf o)) {} applied;
    };

  cargoHome = packages: runCommand "cargo-home" { buildInputs = [cargo]; } ''
    mkdir -p $out
    cp "${withPackages.cargoNix packages}"/Cargo.toml $out
    cp "${withPackages.cargoNix packages}"/Cargo.lock $out

    mkdir -p $out/src
    touch $out/src/lib.rs

    mkdir -p $out/.cargo
    cat <<EOT >> $out/.cargo/config.toml
    [source.crates-io]
    replace-with = "vendored-sources"

    [source.vendored-sources]
    directory = "${withPackages.vendorDependencies packages}"

    [net]
    offline = true
    EOT

    # For some reason evcxr seems to only find config.toml when it's here,
    # even though the cargo docs say it should be inside a .cargo folder:
    ln -s $out/.cargo/config.toml $out/config.toml

    cd $out
    cargo metadata --offline >> metadata.json
  '';

  evcxrConfigDir = packages:
    let
      deps = withPackages.vendorDependencies packages;
    in
      runCommand "evcxr-config" { buildInputs = [python3]; } ''
        mkdir -p $out
        echo ":offline 1" >> $out/init.evcxr
        python ${./python}/build_init_evcxr.py \
          '${deps}'\
          '${lib.generators.toJSON {} packages}' \
          "${cargoHome packages}/metadata.json" \
          "$out/init.evcxr"
      '';

in

evcxr.overrideAttrs (oldAttrs: {
  passthru = (oldAttrs.passthru or {}) // {
    cratesIndex = withPackages.cratesIndex;

    withPackages = packages:
      let
        inherit (overrideInputs packages) nativeBuildInputs buildInputs env;
        # Colon-separated so it survives the word-splitting of $makeWrapperArgs.
        # pkg-config emits the right -L/-I flags from these .pc files, so this is
        # all the build scripts need to find and link the libraries.
        pkgConfigPath = lib.concatStringsSep ":" (lib.concatMap (l: [
          "${lib.getDev l}/lib/pkgconfig"
          "${lib.getDev l}/share/pkgconfig"
        ]) buildInputs);
        # Loadable at runtime, for build scripts that link native libs (e.g.
        # clang-sys -> libclang) and for user code that loads them when it runs.
        libraryPath = lib.makeLibraryPath buildInputs;
        # env-var overrides (LIBCLANG_PATH, ...). Values are store paths / flags
        # with no spaces, so they pass through $makeWrapperArgs word-splitting.
        envArgs = lib.concatLists (lib.mapAttrsToList (n: v: ["--set" n v]) env);
      in
      runCommand "evcxr" {
        version = evcxr.version;

        buildInputs = [makeWrapper];

        # evcxr links user code at *runtime* outside any Nix derivation, so the
        # native deps that stdenv would normally provide during a build are
        # missing. We bake into the wrapper:
        #   - the override-derived tools (pkg-config, cmake, ...) on PATH
        #   - pkg-config search paths for the override-derived libraries
        #   - libiconv on Darwin (rustc emits `-liconv` with no `-L` otherwise)
        makeWrapperArgs = [
          "--set" "CARGO_HOME" "${cargoHome packages}"
          "--set" "EVCXR_CONFIG_DIR" "${evcxrConfigDir packages}"
          "--prefix" "PATH" ":" "${lib.makeBinPath ([rustc cargo] ++ nativeBuildInputs)}"
        ] ++ envArgs ++ lib.optionals (buildInputs != []) [
          "--suffix" "PKG_CONFIG_PATH" ":" "${pkgConfigPath}"
          "--suffix" "LD_LIBRARY_PATH" ":" "${libraryPath}"
        ] ++ lib.optionals stdenv.isDarwin [
          "--set" "NIX_LDFLAGS_${stdenv.cc.suffixSalt}" "-L${lib.getLib libiconv}/lib"
        ];

        passthru = {
          cargoHome = cargoHome packages;
        };
      } ''
        mkdir -p $out/bin
        makeWrapper ${evcxr}/bin/evcxr $out/bin/evcxr $makeWrapperArgs
        makeWrapper ${evcxr}/bin/evcxr_jupyter $out/bin/evcxr_jupyter $makeWrapperArgs
      '';
  };
})
