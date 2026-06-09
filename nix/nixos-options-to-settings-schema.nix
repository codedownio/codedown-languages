{ lib, ... }@args:

{
  componentsToDrop ? 0
}:

options:

let
  convert = v: let
    convertType = import ./convert-type.nix args;

    defaultItem = { type = { name = "unknown"; }; };
    loc = lib.drop componentsToDrop v.loc;

    opt = lib.attrByPath loc null options;
    # A "title" attribute can be attached to an option after mkOption, e.g.
    #   (mkOption {...}) // { title = "Extra Nix"; }
    # (mkOption itself rejects unknown args, but extra attrs survive evalModules.)
    customTitle = opt.title or null;
    example = opt.example or null;
    hasCustomTitle = builtins.typeOf customTitle == "string";
    exampleIsString = builtins.typeOf example == "string";

    value = { inherit loc; }
      // convertType v.name (lib.attrByPath loc defaultItem options).type
      // lib.optionalAttrs (lib.hasAttr "default" v) { defaultValue = opt.default; }
      # title comes from the explicit "title" attr if present, otherwise (for backwards
      # compatibility) from "example".
      // lib.optionalAttrs hasCustomTitle { title = customTitle; }
      // lib.optionalAttrs (!hasCustomTitle && exampleIsString) { title = example; }
      # When a title is given explicitly, "example" is freed up to act as a placeholder.
      // lib.optionalAttrs (hasCustomTitle && exampleIsString) { placeholder = example; }
      // lib.optionalAttrs (lib.hasAttr "description" v && builtins.typeOf v.description == "string") { inherit (v) description; }
      // lib.optionalAttrs (lib.hasAttr "visible" v && v.visible == false) { hidden = true; };
  in
    { name = lib.concatStringsSep "." loc; inherit value; };

in

lib.listToAttrs (map convert (lib.optionAttrSetToDocList options))
