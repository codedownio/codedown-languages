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

  # Compiler generations to include.
  # We'll search through the compilers to find the latest GHC version in each of these series.
  compilerGenerations = [
    ["7" "8"]
    ["7" "10"]
    ["8" "0"]
    ["8" "1"]
    ["8" "2"]
    ["8" "4"]
    ["8" "6"]
    ["8" "8"]
    ["8" "10"]
    ["9" "0"]
    ["9" "2"]
    ["9" "4"]
  ];

  tuplify = s: if (stringLength s) == 4 then
    [(substring 0 1 s) (substring 1 2 s) (substring 3 1 s)]
    else
    [(substring 0 1 s) (substring 1 1 s) (substring 2 1 s)]
  ;
  toGenerationTuple = s: tuplify (removePrefix "ghc" s);
  foundCompilerGenerations = map toGenerationTuple (attrNames compilers);

  # Find max compiler by generations
  generationMatches = gen: foundGen: ((elemAt gen 0) == (elemAt foundGen 0)) && ((elemAt gen 1) == (elemAt foundGen 1));

  maximumGen = list: foldl (x: y: if common.lexicographyVersionNumber (concatStringsSep "." x) > common.lexicographyVersionNumber (concatStringsSep "." y) then x else y) (head list) list;
  maxCompilerByGeneration = listToAttrs (filter (x: x != null) (map (gen:
    let
      matchingGenerations = filter (generationMatches gen) foundCompilerGenerations;
    in
      if matchingGenerations == [] then null
      else (
        let
          maxGen = maximumGen matchingGenerations;
          maxGenName = "ghc" + (concatStrings maxGen);
        in
        {
          name = maxGenName;
          value = getAttr maxGenName compilers;
        }
      )
  ) foundCompilerGenerations));

in

writeTextFile {
  name = "haskell-compiler-snapshots.nix";
  text = toPretty {} maxCompilerByGeneration;
}
