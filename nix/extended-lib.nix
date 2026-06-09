# lib extended with the bits the codedown module system relies on:
#  - the codeMirrorLines option type
#  - support for a human-readable "title" attribute on options
{ lib }:

lib.extend (final: prev: {
  types = prev.types // {
    codeMirrorLines = mode: prev.mkOptionType {
      name = "codeMirrorLines";
      description = "string (${mode})";
      check = builtins.isString;
      merge = prev.options.mergeEqualOption;
    } // { codeMirrorMode = mode; };
  };

  # Allow attaching a human-readable "title" to an option, e.g.
  #   foo = mkOption { title = "Foo"; type = types.str; ... };
  # lib.mkOption rejects unknown arguments, so we strip "title" before calling it and re-attach
  # it afterwards. The attribute survives evalModules and is read by
  # nix/nixos-options-to-settings-schema.nix to drive the settings UI.
  mkOption = args:
    (prev.mkOption (builtins.removeAttrs args ["title"]))
    // prev.optionalAttrs (args ? title) { inherit (args) title; };
})
