{ lib
, writeTextFile
, checks
}:


with lib;

let
  names = attrNames checks;

  buildJob = name: {
    name = name;
    value = {
      name = name;
      runs-on = "self-hosted";

      steps = [
        { uses = "actions/checkout@v2";
          "with" = {
            submodules = "recursive";
            persist-credentials = false;
          };
        }
      ];
    };
  };

in

writeTextFile {
  name = "checks.yaml";
  text = lib.generators.toYAML {} {
    name = "ci";

    on = {
      pull_request = {};
      push = {
        branches = ["main"];
      };
    };

    jobs = {
      tests = {
        name = "\${{matrix.derivation}}";
        runs-on = "self-hosted";

        strategy = {
          fail-fast = false;
          matrix = {
            include = map (x: {
              derivation = x;
            }) names;
          };
        };

        steps = [{
          uses = "actions/checkout@v2";
          "with" = {
            submodules = "recursive";
            persist-credentials = false;
          };
        } {
          name = "Test";
          run = ''
            set -eo pipefail

            derivation=''${{matrix.derivation}}
            echo "Got derivation: $derivation"

            output=$(nix build .#checks.x86_64-linux.''${derivation} --no-link --json --extra-experimental-features nix-command --extra-experimental-features flakes | jq -r '.[0].outputs.out')
            echo "Got output: $output"

            if [[ -f "$output" && -x $(realpath "$output") ]]; then
              echo "Output is executable!"
              $output
            fi
          '';
        }];
      };
    };
  };
}
