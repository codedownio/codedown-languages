{ lib, ... }@args:

{
  componentsToDrop ? 0
}:

options:

let
  inherit (import ./convert-type.nix args) convertOption;

  convert = v: let
    loc = lib.drop componentsToDrop v.loc;
  in {
    name = lib.concatStringsSep "." loc;
    value = { inherit loc; } // convertOption v.name (lib.getAttrFromPath loc options);
  };

  # The doc list also has entries like "servers.<name>.port"; the parent's schema covers those.
  isTopLevel = v: !(lib.any (c: c == "<name>" || c == "*") v.loc);

in

lib.listToAttrs (map convert (lib.filter isTopLevel (lib.optionAttrSetToDocList options)))
