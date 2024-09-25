{ lib }:

settingsSchema:

settings:

with lib;

let
  evalString = str: builtins.scopedImport {} (builtins.toFile "expr.nix" str);

  getSettingsRow = schemaItem: value:
    if value != schemaItem.defaultValue
    then { name = concatStringsSep "." schemaItem.target; inherit value; }
    else null
  ;

  tryGetSettingsRow = schemaItem:
    if hasAttrByPath schemaItem.target settings
    then getSettingsRow schemaItem (getAttrFromPath schemaItem.target settings)
    else null
  ;

in

listToAttrs (filter (x: x != null) (map tryGetSettingsRow settingsSchema))
