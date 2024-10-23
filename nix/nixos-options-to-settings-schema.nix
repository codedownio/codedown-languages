{ lib, ... }@args:

{
  componentsToDrop ? 0
}:

options:

let
  convert = v: let
    convertType = import ./convert-type.nix args;
    evalString = str: builtins.scopedImport {} (builtins.toFile "expr.nix" str);

    convertDefaultValue = value:
      if value._type == "literalExpression" then evalString value.text
      else builtins.throw "Can't handle this default value: ${toString value}.";

    defaultItem = { type = { name = "unknown"; }; };
    loc = lib.drop componentsToDrop v.loc;

    value = { inherit loc; }
      // convertType v.name (lib.attrByPath loc defaultItem options).type
      // lib.optionalAttrs (lib.hasAttr "default" v) { defaultValue = convertDefaultValue v.default; }
      // (let example = (lib.attrByPath loc null options).example or null; in lib.optionalAttrs (builtins.typeOf example == "string") { title = example; })
      // lib.optionalAttrs (lib.hasAttr "description" v && builtins.typeOf v.description == "string") { inherit (v) description; }
      // lib.optionalAttrs (lib.hasAttr "visible" v && v.visible == false) { hidden = true; };
  in
    { name = lib.concatStringsSep "." loc; inherit value; };

in

lib.listToAttrs (map convert (lib.optionAttrSetToDocList options))
