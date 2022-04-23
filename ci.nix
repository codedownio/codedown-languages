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
            derivation=''${{matrix.derivation}}

            echo "Got derivation: $derivation"

            nix build .#checks.x86_64-linux.''${derivation} --no-link --json
          '';
        }];
      };
    };
  };
}
