{ lib
, writeTextFile
, checks
}:

with lib;

writeTextFile {
  name = "checks.yaml";
  text = ''
    Got check names: ${builtins.concatStringsSep ", " (attrNames checks)}
  '';
}
