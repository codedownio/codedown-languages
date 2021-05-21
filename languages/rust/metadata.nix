{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseCandidates = [
    "rust_1_40"
    "rust_1_41"
    "rust_1_42"
    "rust_1_43"
    "rust_1_44"
    "rust_1_45"
    "rust_1_46"
    "rust_1_47"
    "rust_1_48"
    "rust_1_49"
    "rust_1_50"
    "rust_1_51"
    "rust_1_52"
    "rust_1_53"
    "rust_1_54"
    "rust_1_55"
    "rust_1_56"
    "rust_1_57"
    "rust_1_58"
    "rust_1_59"
    "rust_1_60"
  ];
  baseOptions = map (x:
    let rust = getAttr x pkgs; in {
      # rust = rust.packages.stable.rustc;
      inherit rust;
      name = x;
      displayName = "Rust " ;
      meta = {}; # rust.meta;
      logo = ./logo-64x64.png;
    }
  ) (filter (x: hasAttr x pkgs) baseCandidates);

  packageOptions = base@{rust, ...}: {};

  languageServerOptions = base@{rust, ...}: packages: {};
}
