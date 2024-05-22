{ lib
, runCommand
, callPackage
, makeWrapper
, stdenv
}:

rec {
  wrapShell = {
    executableName
    , baseDerivation
    , displayName
    , attr
    , icon
  }: stdenv.mkDerivation {
    name = baseDerivation.name;
    version = baseDerivation.version;

    dontUnpack = true;

    inherit baseDerivation executableName;
    buildInputs = [makeWrapper];

    buildPhase = ''
      mkdir -p $out/lib/codedown
      makeWrapper "$baseDerivation/bin/$executableName" $out/lib/codedown/shell
    '';

    dontInstall = true;

    meta = baseDerivation.meta // {
      inherit icon displayName;

      # To separate these out in search results
      category = "Shells";
    };

    inherit icon displayName attr;
  };

  wrapShells = allShells: runCommand "codedown-shells" { shells = (map (x: x.contents) allShells); } ''
    mkdir -p $out/lib/codedown/shells

    COUNTER=1
    for shell in $shells; do
      ln -s $shell/lib/codedown/shell $out/lib/codedown/shells/shell$COUNTER
      let COUNTER++
    done

    if [[ -f "$out/lib/codedown/shells/shell1" ]]; then
      ln -s $out/lib/codedown/shells/shell1 $out/lib/codedown/shell
    fi
  '';
}
