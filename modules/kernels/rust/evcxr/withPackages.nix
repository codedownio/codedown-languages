{ callPackage
, fetchFromGitHub
, lib
, python3
, runCommand
, writeTextFile

, cargo
, rustPlatform
}:

let
  # nix-prefetch-github rust-lang crates.io-index
  cratesIndex = fetchFromGitHub {
    owner = "rust-lang";
    repo = "crates.io-index";
    rev = "11e3fcf4f37f7540f34ef3681a3f0a2a11d69233";
    sha256 = "sha256-xKs7GMldNEqAqdBwsmdqc5fxHE6oXKQBp5usVTfUZxo=";
  };

  allPackageNames = runCommand "rust-package-names.nix" {} ''
    echo "[" >> $out

    find "${cratesIndex}" -type f -exec basename {} \; \
      | awk '{ print "\""$0"\""}' \
      | sort \
      >> $out

    echo "]" >> $out
  '';

  # Map of crate name -> available Cargo features, parsed from the index. Lets the
  # kernel surface each package's features as a multi-select in settings. Built
  # once per index revision (~40k crates that have features, a few MB of JSON).
  cratesFeatures = runCommand "crates-features.json" { nativeBuildInputs = [python3]; } ''
    python3 ${./python}/build_crates_features.py "${cratesIndex}" "$out"
  '';

  renderPackage = pn:
    if builtins.isString pn then ''${pn} = "*"''
    else if builtins.isAttrs pn && lib.hasAttrByPath ["features"] pn
      then ''${pn.name} = { version = "*", features = [${lib.concatStringsSep ", " (map (feat: ''"'' + feat + ''"'') pn.features)}] }''
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

    mkdir -p $out

    cargo generate-lockfile --offline

    echo "{rustPlatform}: rustPlatform.importCargoLock { lockFile = ./Cargo.lock; }" >> default.nix

    # Emit the full resolved crate list (including transitive deps) as a Nix list,
    # so callers can intersect it with nixpkgs' defaultCrateOverrides to discover
    # which native build inputs are needed to compile this dependency set.
    {
      echo "["
      grep '^name = ' Cargo.lock | sed -E 's/^name = "(.*)"/  "\1"/'
      echo "]"
    } >> crate-names.nix

    cp -r Cargo.toml Cargo.lock default.nix crate-names.nix $out
  '';

  vendorDependencies = packages:
    callPackage "${cargoNix packages}" {
      inherit rustPlatform;
    };

in

{
  inherit cratesIndex;

  inherit allPackageNames;

  inherit cratesFeatures;

  inherit cargoToml;

  inherit cargoNix;

  inherit vendorDependencies;
}
