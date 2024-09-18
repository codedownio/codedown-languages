{ config, lib, ... }:

let
  convertType = target: type:
    if (type.name == "str") then "string"
    else if (type.name == "anything") then "any"
    else if (type.name == "bool") then "boolean"
    else if (type.name == "attrs") then {
      tag = "attrs";
    }
    else if (type.name == "listOf") then {
      tag = "list";
      value = convertType target type.nestedTypes.elemType;
    }
    else if (type.name == "enum") then {
      tag = "enum";
      values = type.functor.payload;
    }
    else if (type.name == "either") then {
      tag = "either";
      left = convertType target type.nestedTypes.left;
      right = convertType target type.nestedTypes.right;
    }
    else builtins.throw "Can't convert type for '${target}': ${toString type.name}"
  ;

in

convertType
