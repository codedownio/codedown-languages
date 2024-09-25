{ lib }:

settingsSchema:

settings:

with lib;

let
  evalString = str: builtins.scopedImport {} (builtins.toFile "expr.nix" str);

  getSettingsRow = schemaItem: value:
    if value != schemaItem.value.defaultValue
    then { name = schemaItem.name; inherit value; }
    else null
  ;

  tryGetSettingsRow = schemaItem:
    if hasAttrByPath schemaItem.value.loc settings
    then getSettingsRow schemaItem (getAttrFromPath schemaItem.value.loc settings)
    else null
  ;

in

listToAttrs (filter (x: x != null) (map tryGetSettingsRow (attrsToList settingsSchema)))
