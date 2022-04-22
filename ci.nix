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

    jobs = listToAttrs (map buildJob names);
  };
}
