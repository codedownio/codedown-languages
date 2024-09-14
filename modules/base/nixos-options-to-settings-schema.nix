{ config, lib, pkgs, ... }:

options:

let
  flattenAttrSet = prefix: set:
    builtins.foldl' (acc: name:
      let
        value = set.${name};
        newPrefix = if prefix == "" then name else "${prefix}.${name}";
      in
        if builtins.isAttrs value && value != {}
        then acc // (flattenAttrSet newPrefix value)
        else acc // { ${newPrefix} = value; }
    ) {} (builtins.attrNames set);


  flattened = flattenAttrSet options;

in

lib.mapAttrsToList (n: v: {
  target = n;
  title = "asdf";
  type = "fdsa";
})
