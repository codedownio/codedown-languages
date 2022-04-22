{ lib
, writeTextFile
, bats
, jupyter
, jupyter_path
, codeExecutions
}:

let
  name = "check-code";
  runtimeInputs = [jupyter];

  makeTest = codeExecution: ''
    @test "[${codeExecution.kernel}] ${codeExecution.code} --> ${codeExecution.output}" {
      dir=$(mktemp -d)
      cd "$dir"
      set +e

      echo "${codeExecution.code}" | jupyter run --kernel "${codeExecution.kernel}" > out
      [ "$?" = 0 ]

      set -e
      [ "$(cat out)" = "${codeExecution.output}" ]
    }
  '';

  tests = builtins.concatStringsSep "\n\n" (map makeTest codeExecutions);

in

writeTextFile {
  inherit name;
  executable = true;
  text = ''
    #!${bats}/bin/bats
    set -o errexit
    set -o nounset
    set -o pipefail

    export PATH="${lib.makeBinPath runtimeInputs}:$PATH"
    export JUPYTER_PATH="${jupyter_path}"

    ${tests}
  '';
}
