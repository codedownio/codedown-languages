{ lib, ... }:

let
  convertType = target: type:
    if (type.name == "str") then { type = "string"; }
    else if (type.name == "separatedString") then { type = "lines"; }
    else if (type.name == "anything") then { type = "any"; }
    else if (type.name == "bool") then { type = "boolean"; }
    else if (type.name == "attrs") then {
      type = "attrs";
    }
    else if (type.name == "listOf") then {
      type = "list";
      listType = convertType target type.nestedTypes.elemType;
    }
    else if (type.name == "enum") then {
      type = "enum";
      values = type.functor.payload.values;
    }
    else if (type.name == "either") then {
      type = "either";
      left = convertType target type.nestedTypes.left;
      right = convertType target type.nestedTypes.right;
    }
    else if (type.name == "submodule") then {
      type = "submodule";
      keys = lib.mapAttrsRecursiveCond
        (x: !(x ? _type))
        (path: value: convertType target value.type)
        (lib.removeAttrs (type.getSubOptions {}) ["_module"]);
    }
    # else if (type.name == "unknown") then {
    #   type = "unknown";
    # }
    else builtins.throw "Can't convert type for '${target}': ${toString type.name}"
  ;

in

convertType
