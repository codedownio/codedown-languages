# Checks that nested settings schema items carry a defaultValue the UI can use, e.g. for a list's "Add".

{ lib
, runCommand
, callPackage
}:

let
  t = lib.types;

  options = (lib.evalModules {
    modules = [{
      options.x = {
        names = lib.mkOption { type = t.listOf t.str; default = []; };
        ports = lib.mkOption { type = t.listOf t.port; default = []; };
        counts = lib.mkOption { type = t.listOf (t.ints.between 3 9); default = []; };
        modes = lib.mkOption { type = t.listOf (t.enum ["a" "b"]); default = []; };
        servers = lib.mkOption {
          type = t.attrsOf (t.submodule {
            options.port = lib.mkOption { type = t.ints.positive; };
            options.tls = lib.mkOption { type = t.bool; default = true; description = "Use TLS"; };
          });
          default = {};
        };
        packages = lib.mkOption {
          type = t.listOf (t.either t.str (t.submodule {
            options.name = lib.mkOption { type = t.str; visible = false; };
          }));
          default = [];
        };
      };
    }];
  }).options;

  actual = lib.mapAttrs (_: v: lib.removeAttrs v ["loc" "defaultValue"])
    (callPackage ./nixos-options-to-settings-schema.nix {} { componentsToDrop = 1; } options.x);

  expected = {
    names = { type = "list"; listType = { type = "string"; defaultValue = ""; }; };
    ports = { type = "list"; listType = { type = "integer"; bounds = [0 65535]; defaultValue = 0; }; };
    counts = { type = "list"; listType = { type = "integer"; bounds = [3 9]; defaultValue = 3; }; };
    modes = { type = "list"; listType = { type = "enum"; values = ["a" "b"]; defaultValue = "a"; }; };
    servers = {
      type = "attrs";
      valueType = {
        type = "submodule";
        keys = {
          port = { type = "integer"; };
          tls = { type = "boolean"; defaultValue = true; description = "Use TLS"; };
        };
        defaultValue = { port = 1; tls = true; };
      };
    };
    packages = {
      type = "list";
      listType = {
        type = "either";
        left = { type = "string"; defaultValue = ""; };
        right = {
          type = "submodule";
          keys.name = { type = "string"; hidden = true; };
          defaultValue = { name = ""; };
        };
        defaultValue = "";
      };
    };
  };

in

runCommand "check-settings-schema" {} (
  if actual == expected
  then "touch $out"
  else ''
    echo "Settings schema doesn't match. Expected:"
    echo ${lib.escapeShellArg (builtins.toJSON expected)}
    echo "Actual:"
    echo ${lib.escapeShellArg (builtins.toJSON actual)}
    exit 1
  ''
)
