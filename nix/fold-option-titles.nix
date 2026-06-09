# Folds the custom "title" attribute (see nix/extended-lib.nix) into each option's description,
# so it shows up in the generated docs (OPTIONS.md / options JSON). nixosOptionsDoc only renders
# the standard fields, so without this the title would be lost in the docs.
#
# This is docs-only: the settings-schema generator reads `title` and `description` separately,
# so it should keep using the raw options, not this transformed tree.
{ lib }:

let
  foldOne = opt:
    if (opt ? title) && builtins.isString opt.title
    then opt // {
      description =
        if (opt ? description) && builtins.isString opt.description
        then "**${opt.title}**\n\n${opt.description}"
        else "**${opt.title}**";
    }
    else opt;

  go = opts:
    if lib.isOption opts then foldOne opts
    else if builtins.isAttrs opts then lib.mapAttrs (_: go) opts
    else opts;
in
  go
