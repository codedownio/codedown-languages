{ config, lib, ... }:

options:

let
  flattened = lib.optionAttrSetToDocList options;

  convert = v: {
    target = v.name;
    type = v.type;
    loc = v.loc;
  }
  // lib.optionalAttrs (lib.hasAttr "default" v) { defaultValue = convertDefaultValue v.default; }
  // lib.optionalAttrs (lib.hasAttr "description" v && builtins.typeOf v.description == "string") { description = v.description; }
  ;

  evalString = str: builtins.scopedImport {} (builtins.toFile "expr.nix" str);

  convertDefaultValue = value:
    if value._type == "literalExpression" then builtins.toJSON (evalString value.text)
    else builtins.throw "Can't handle this default value: ${toString value}.";

in

map convert flattened
