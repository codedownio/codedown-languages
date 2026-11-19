{ lib, ... }:

let
  isInt = type:
    builtins.elem type.name ["int" "unsignedInt" "positiveInt" "intBetween"]
    || builtins.match "(un)?signedInt[0-9]+" type.name != null;

  # nixpkgs keeps an int type's bounds only in its description.
  intBounds = type: let
    m = builtins.match ".*between (-?[0-9]+) and (-?[0-9]+) \\(both inclusive\\)" type.description;
  in
    if m == null then null else map lib.toInt m;

  subOptions = type: lib.removeAttrs (type.getSubOptions []) ["_module"];

  mapSubOptions = f: type: lib.mapAttrsRecursiveCond (x: !(x ? _type)) (_path: f) (subOptions type);

  # What a new value of this type starts as, e.g. when "Add" is clicked on a list.
  typeDefault = type:
    if builtins.elem type.name ["str" "separatedString" "codeMirrorLines"] then ""
    else if type.name == "bool" then false
    else if isInt type then (
      let bounds = intBounds type; in
      if bounds != null then lib.max (lib.head bounds) (lib.min 0 (lib.last bounds))
      else if type.check 0 then 0
      else 1
    )
    else if type.name == "enum" then lib.head (type.functor.payload.values ++ [null])
    else if type.name == "listOf" then []
    else if builtins.elem type.name ["attrs" "attrsOf"] then {}
    else if type.name == "either" then typeDefault type.nestedTypes.left
    else if type.name == "submodule" then mapSubOptions (opt: opt.default or (typeDefault opt.type)) type
    else null;

  convertType = target: type:
    if (type.name == "str") then { type = "string"; }
    else if (type.name == "separatedString") then { type = "lines"; }
    else if (type.name == "codeMirrorLines") then { type = "lines"; codeMirrorMode = type.codeMirrorMode; }
    else if (type.name == "anything") then { type = "any"; }
    else if (type.name == "bool") then { type = "boolean"; }
    else if (isInt type) then {
      type = "integer";
    } // lib.optionalAttrs (intBounds type != null) { bounds = intBounds type; }
    else if (type.name == "attrs") then {
      type = "attrs";
    }
    else if (type.name == "attrsOf") then {
      type = "attrs";
      valueType = convertNested target type.nestedTypes.elemType;
    }
    else if (type.name == "listOf") then {
      type = "list";
      listType = convertNested target type.nestedTypes.elemType;
    }
    else if (type.name == "enum") then {
      type = "enum";
      values = type.functor.payload.values;
    }
    else if (type.name == "either") then {
      type = "either";
      left = convertNested target type.nestedTypes.left;
      right = convertNested target type.nestedTypes.right;
    }
    else if (type.name == "submodule") then {
      type = "submodule";
      keys = mapSubOptions (convertOption target) type;
    }
    else builtins.throw "Can't convert type for '${target}': ${toString type.name}"
  ;

  convertNested = target: type: convertType target type // { defaultValue = typeDefault type; };

  convertOption = target: opt: let
    # A "title" attribute can be attached to an option after mkOption, e.g.
    #   (mkOption {...}) // { title = "Extra Nix"; }
    # (mkOption itself rejects unknown args, but extra attrs survive evalModules.)
    customTitle = opt.title or null;
    example = opt.example or null;
    description = opt.description or null;
    hasCustomTitle = builtins.typeOf customTitle == "string";
    exampleIsString = builtins.typeOf example == "string";
  in
    convertType target opt.type
    // lib.optionalAttrs (opt ? default) { defaultValue = opt.default; }
    # title comes from the explicit "title" attr if present, otherwise (for backwards
    # compatibility) from "example".
    // lib.optionalAttrs hasCustomTitle { title = customTitle; }
    // lib.optionalAttrs (!hasCustomTitle && exampleIsString) { title = example; }
    # When a title is given explicitly, "example" is freed up to act as a placeholder.
    // lib.optionalAttrs (hasCustomTitle && exampleIsString) { placeholder = example; }
    // lib.optionalAttrs (builtins.typeOf description == "string") { inherit description; }
    // lib.optionalAttrs ((opt.visible or true) == false) { hidden = true; };

in

{
  inherit convertOption;
}
