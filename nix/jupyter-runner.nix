{ coreutils
, findutils
, lib
, makeWrapper
, python3
, runCommand
}:

let
  pythonEnv = python3.withPackages (ps: with ps; [papermill]);
  packages = [coreutils findutils pythonEnv];

in

runCommand "papermill" { buildInputs = [makeWrapper]; } ''
  makeWrapper ${pythonEnv}/bin/papermill $out \
    --set PATH ${lib.makeBinPath packages}
''
