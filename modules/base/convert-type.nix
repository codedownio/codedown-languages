{ config, lib, ... }:

let
  convertType = type:
    if (type.name == "str") then "string"
    else if (type.name == "anything") then "any"
    else if (type.name == "bool") then "boolean"
    else if (type.name == "listOf") then "[" + (convertType type.nestedTypes.elemType) + "]"
    else if (type.name == "enum") then "(" + "asdf" + ")"
    else builtins.throw "Can't convert type: ${toString type.name}"
  ;

in

convertType
