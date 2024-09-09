{ callPackage
, fetchFromGitHub
, lib
, runCommand
, writeTextFile

, python3

, cargo
, rustPlatform
}:

let
  # nix-prefetch-github rust-lang crates.io-index
  cratesIndex = fetchFromGitHub {
    owner = "rust-lang";
    repo = "crates.io-index";
    rev = "b6c0211bd5c02fcff4ae199e7efcf8c2e087e239";
    sha256 = "sha256-iJCUHbQoOYoXj0a/qvn4OxQdcnahukm+zOhsXlQI+Hs=";
  };

  allPackageNames = runCommand "rust-package-names.nix" {} ''
    echo "[" >> $out

    find "${cratesIndex}" -type f -exec basename {} \; \
      | awk '{ print "\""$0"\""}' \
      | sort \
      >> $out

    echo "]" >> $out
  '';

  renderPackage = pn:
    if builtins.isString pn then ''${pn} = "*"''
    else if builtins.isAttrs pn && lib.hasAttrByPath ["settings" "features"] pn
      then ''${pn.name} = { version = "*", features = [${lib.concatStringsSep ", " (map (feat: ''"'' + feat + ''"'') pn.settings.features)}] }''
      else ''${pn.name} = { version = "*" }'';

  cargoToml = packages: writeTextFile {
    name = "Cargo.toml";
    text = ''
      [package]
      name = "rust_environment"
      version = "0.1.0"
      edition = "2018"

      [dependencies]
    '' + lib.concatStringsSep "\n" (map renderPackage packages);
  };

  cargoNix = packages: runCommand "Cargo-nix" { buildInputs = [cargo]; } ''
    cp "${cargoToml packages}" Cargo.toml

    # Generate lib.rs
    mkdir -p "src"
    touch src/lib.rs

    # Set up fake index
    mkdir -p index_base
    ln -s ${cratesIndex} index_base/index

    # Set up HOME with cargo config
    mkdir -p home/.cargo
    export HOME=$(pwd)/home
    cat <<EOT >> home/.cargo/config
    [source.crates-io]
    local-registry = "$(pwd)/index_base"
    EOT

    cargo generate-lockfile --offline

    echo "{rustPlatform}: rustPlatform.importCargoLock { lockFile = ./Cargo.lock; }" >> default.nix

    mkdir -p $out
    cp -r Cargo.toml Cargo.lock default.nix $out
  '';

  vendorDependencies = packages:
    callPackage "${cargoNix packages}" {
      inherit rustPlatform;
    };

  evcxrConfigDir = packages:
    let
      deps = vendorDependencies packages;
    in
      runCommand "evcxr-config" { buildInputs = [(python3.withPackages (ps: [ps.toml]))]; } ''
        mkdir -p $out
        echo ":offline 1" >> $out/init.evcxr
        python ${./python}/build_init_evcxr.py \
          '${deps}'\
          '${lib.generators.toJSON {} packages}' \
          "${cargoNix packages}/Cargo.lock" \
          "$out/init.evcxr"
      '';

in

rec {
  inherit cratesIndex;

  inherit allPackageNames;

  inherit cargoToml;

  inherit cargoNix;

  inherit vendorDependencies;

  inherit evcxrConfigDir;
}
