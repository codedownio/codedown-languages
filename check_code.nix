{ lib
, writeTextFile
, bats
, jupyter_path
, codeExecutions

, python3
, coreutils
, findutils
}:

let
  name = "check-code";
  runtimeInputs = [
    (python3.withPackages (ps: [ps.jupyter ps.jupyter_client]))
    coreutils
    findutils
  ];

  escape = lib.replaceStrings ["'" "\n"] ["'\\''" "\\n"];

  makeTest = codeExecution: ''
    @test "[${codeExecution.kernel}] '${escape codeExecution.code}' --> ${escape codeExecution.output}" {
      dir=$(mktemp -d)
      cd "$dir"
      set +e

      echo '${escape codeExecution.code}' | jupyter run --kernel "${codeExecution.kernel}" > out
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

    export PATH="${lib.makeBinPath runtimeInputs}"
    export JUPYTER_PATH="${jupyter_path}"

    ${tests}
  '';
}
