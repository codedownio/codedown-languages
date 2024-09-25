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
    loc = lib.drop componentsToDrop v.loc;
  in
    {
      target = loc;
    }
    // convertType v.name (lib.attrByPath loc defaultItem options).type
    // lib.optionalAttrs (lib.hasAttr "default" v) { defaultValue = convertDefaultValue v.default; }
    // (let example = (lib.attrByPath loc null options).example or null; in lib.optionalAttrs (builtins.typeOf example == "string") { title = example; })
    // lib.optionalAttrs (lib.hasAttr "description" v && builtins.typeOf v.description == "string") { inherit (v) description; }
    // lib.optionalAttrs (lib.hasAttr "visible" v && v.visible == false) { hidden = true; }
  ;

  evalString = str: builtins.scopedImport {} (builtins.toFile "expr.nix" str);

  convertDefaultValue = value:
    if value._type == "literalExpression" then builtins.toJSON (evalString value.text)
    else builtins.throw "Can't handle this default value: ${toString value}.";

in

map convert flattened
