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
, evcxr
, rustPlatform
, rustc
}:

let
  withPackages = callPackage ./withPackages.nix {
    inherit cargo rustPlatform;
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
      runCommand "evcxr-config" { buildInputs = [(python3.withPackages (ps: [ps.toml]))]; } ''
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
      runCommand "evcxr" {
        version = evcxr.version;

        buildInputs = [makeWrapper];

        # On Darwin, rustc invokes `cc` for linking at runtime. cc is the Nix
        # clang wrapper, which only adds library paths it has been told about
        # via NIX_LDFLAGS_<suffix>. libiconv is normally pulled in by stdenv
        # buildInputs during a Nix build, but evcxr links user code at *runtime*
        # outside any derivation, so the wrapper sees no NIX_LDFLAGS and rustc
        # ends up emitting `-liconv` with no `-L` for it. Bake the libiconv
        # path into the wrapper here so runtime linking finds it.
        makeWrapperArgs = [
          "--set" "CARGO_HOME" "${cargoHome packages}"
          "--set" "EVCXR_CONFIG_DIR" "${evcxrConfigDir packages}"
          "--prefix" "PATH" ":" "${lib.makeBinPath [rustc cargo]}"
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
