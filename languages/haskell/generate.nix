{ lib
, callPackage
, stdenv
, symlinkJoin
, makeWrapper

, writeTextFile

, haskell
, haskell-language-server
, haskell-nix
, filterToValid ? false
, ltsOnly ? true
}:

with callPackage ./inputs.nix {};

with lib;
with lib.generators;

let
  common = callPackage ../common.nix {};
  util = callPackage ./util.nix {};

  maximumByVersion = list: foldl (x: y: if common.lexicographyVersionNumber (util.getVersion x) > common.lexicographyVersionNumber (util.getVersion y) then x else y) (head list) list;

  compilers = zipAttrsWith (name: values: maximumByVersion values) (mapAttrsToList (name: value:
    let
      snapshot = (getAttr name haskell-nix.stackage) haskell-nix.hackage;
    in
      listToAttrs [(nameValuePair snapshot.compiler.nix-name name)]
  ) haskell-nix.snapshots);

in

writeTextFile {
  name = "haskell-compiler-snapshots.nix";
  text = toPretty {} compilers;
}
