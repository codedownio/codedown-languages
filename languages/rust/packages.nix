{ callPackage
, fetchFromGitHub
, runCommand

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
    echo '[package]' >> Cargo.toml
    echo 'name = "rust_environment"' >> Cargo.toml
    echo 'version = "0.1.0"' >> Cargo.toml
    echo 'edition = "2018"' >> Cargo.toml
    echo "" >> Cargo.toml
    echo '[dependencies]' >> Cargo.toml
    for name in ${toString packageNames}; do
      echo "$name = \"*\"" >> Cargo.toml
    done

    # Generate lib.rs
    mkdir -p "src"
    echo "" >> src/lib.rs

    # Set up fake index
    mkdir -p index_base
    ln -s ${cratesIndex} index_base/index

    # Set up HOME with cargo config
    mkdir -p home/.cargo
    export HOME=$(pwd)/home
    echo '[source.crates-io]' >> home/.cargo/config
    echo "local-registry = \"$(pwd)/index_base\"" >> home/.cargo/config

    cargo generate-lockfile --offline

    echo "{rustPlatform}: rustPlatform.importCargoLock { lockFile = ./Cargo.lock; }" >> default.nix

    mkdir -p $out
    cp -r Cargo.toml Cargo.lock default.nix $out
  '';

in

rec {
  inherit cratesIndex;

  vendorDependencies = packageNames:
    callPackage "${cargoNix packageNames}" {
      inherit rustPlatform;
    };

  inherit allPackageNames;

  foo = vendorDependencies ["rand" "quote"];
}
