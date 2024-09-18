{ config, lib, ... }@args:

{
  componentsToDrop ? 0
}:

options:

let
  convertType = import ./convert-type.nix args;

  flattened = lib.optionAttrSetToDocList options;

  convert = v: let
    defaultItem = { type = { name = "unknown"; }; };
  in
    {
      target = v.name;
      type = convertType v.name (lib.attrByPath (lib.drop componentsToDrop v.loc) defaultItem options).type;
      loc = v.loc;
    }
    // lib.optionalAttrs (lib.hasAttr "default" v) { defaultValue = convertDefaultValue v.default; }
    // lib.optionalAttrs (lib.hasAttr "description" v && builtins.typeOf v.description == "string") { inherit (v) description; }
  ;

  evalString = str: builtins.scopedImport {} (builtins.toFile "expr.nix" str);

  convertDefaultValue = value:
    if value._type == "literalExpression" then builtins.toJSON (evalString value.text)
    else builtins.throw "Can't handle this default value: ${toString value}.";

in

map convert flattened
