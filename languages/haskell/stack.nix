with import <nixpkgs> {};

runCommand "wrapped-stack" {
  stack = haskellPackages.stack;
  buildInputs = [makeWrapper];
  propagatedBuildInputs = [xz gnumake];
} ''
  mkdir -p $out/bin
  makeWrapper ${stack}/bin/stack $out/bin/stack --add-flags "--system-ghc" \
                                                --suffix PATH ":" ${haskell.packages.ghc883.ghc}/bin
''
