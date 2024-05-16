{ callPackage
, lib
, makeWrapper
, runCommand

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

  cargoHome = packageNames: runCommand "cargo-home" {} ''
    mkdir -p $out
    cp "${withPackages.cargoNix packageNames}"/Cargo.toml $out
    cp "${withPackages.cargoNix packageNames}"/Cargo.lock $out

    mkdir -p $out/src
    touch $out/src/lib.rs

    mkdir -p $out/.cargo
    cat <<EOT >> $out/.cargo/config.toml
    [source.crates-io]
    replace-with = "vendored-sources"

    [source.vendored-sources]
    directory = "${withPackages.vendorDependencies packageNames}"

    [net]
    offline = true
    EOT

    # For some reason evcxr seems to only find config.toml when it's here,
    # even though the cargo docs say it should be inside a .cargo folder:
    ln -s $out/.cargo/config.toml $out/config.toml
  '';

in

evcxr.overrideAttrs (oldAttrs: {
  passthru = (oldAttrs.passthru or {}) // {
    cratesIndex = withPackages.cratesIndex;

    withPackages = packageNames:
      runCommand "evcxr" {
        version = evcxr.version;

        buildInputs = [makeWrapper];

        makeWrapperArgs = [
          "--set" "EVCXR_CONFIG_DIR" "${withPackages.evcxrConfigDir packageNames}"
          "--set" "CARGO_HOME" "${cargoHome packageNames}"
          "--prefix" "PATH" ":" "${lib.makeBinPath [rustc cargo]}"
        ];

        passthru = {
          cargoHome = cargoHome packageNames;
        };
      } ''
        mkdir -p $out/bin
        makeWrapper ${evcxr}/bin/evcxr $out/bin/evcxr $makeWrapperArgs
        makeWrapper ${evcxr}/bin/evcxr_jupyter $out/bin/evcxr_jupyter $makeWrapperArgs
      '';
  };
})
