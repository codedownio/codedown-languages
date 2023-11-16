{ lib }:

with lib;

rec {
  getVersion = name: if hasPrefix "lts-" name then "b." + (removePrefix "lts-" name)
                     else if hasPrefix "nightly-" name then "a." + (removePrefix "nightly-" name)
                     else name;

  applyVersionToSnapshot = name: snapshot: let
    version = getVersion name; in snapshot // { inherit version; };
}
