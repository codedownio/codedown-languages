{ callPackage
, fetchFromGitHub
, lib
, runCommand

, python3

, cargo
, rustPlatform
}:

let

  cratesIndex = fetchFromGitHub {
    owner = "rust-lang";
    repo = "crates.io-index";
    rev = "d3be09973f4126cda5f8e8e77402e93be5f11bfe";
    sha256 = "1pnsk6a7w71mv792rgf6l5jn3di0zmdibv8h4vbh0jwjr2w37yf2";
  };

  allPackageNames = runCommand "rust-package-names.nix" {} ''
    echo "[" >> $out

    find "${cratesIndex}" -type f -exec basename {} \; \
      | awk '{ print "\""$0"\""}' \
      | sort \
      >> $out

    echo "]" >> $out
  '';

  cargoNix = packageNames: runCommand "rust-dependencies" { buildInputs = [cargo]; } ''
    # Generate Cargo.toml
    cat <<EOT >> Cargo.toml
    [package]
    name = "rust_environment"
    version = "0.1.0"
    edition = "2018"

    [dependencies]
    EOT
    for name in ${toString packageNames}; do
      echo "$name = \"*\"" >> Cargo.toml
    done

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

  vendorDependencies = packageNames:
    callPackage "${cargoNix packageNames}" {
      inherit rustPlatform;
    };

  evcxrConfigDir = packageNames:
    let
      deps = vendorDependencies packageNames;
    in
      runCommand "init-evcxr-dir" { buildInputs = [(python3.withPackages (ps: [ps.toml]))]; } ''
        mkdir -p $out
        python ${./build_init_evcxr.py} '${deps}' '${lib.generators.toJSON {} packageNames}' "$out"
      '';

in

rec {
  inherit cratesIndex;

  inherit allPackageNames;

  inherit vendorDependencies;

  inherit evcxrConfigDir;
}
