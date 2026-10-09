# python-lsp-server always advertises documentFormattingProvider, but it can only actually
# format if one of its formatter plugins is installed -- none of which comes with the base
# package. This turns the kernel's `lsp.python-lsp-server.formatter` setting into the extra
# Python packages to add and the pylsp plugin settings to send.
#
# Every plugin that isn't the chosen one is disabled explicitly: autopep8 registers its hook
# `tryfirst` and both it and yapf default to enabled, so a formatter that happens to be in the
# user's environment would otherwise win over the one they picked.

{ formatter }:

let
  all = ["autopep8" "yapf" "black" "ruff"];

  packagesFor = {
    none = _ps: [];
    autopep8 = ps: [ps.autopep8];
    yapf = ps: [ps.yapf ps.whatthepatch];
    black = ps: [ps.python-lsp-black];
    ruff = ps: [ps.python-lsp-ruff];
  };

  enabled = name: {
    enabled = formatter == name;
  } // (if name == "ruff" then { formatEnabled = formatter == "ruff"; } else {});

in

{
  packages = packagesFor.${formatter};

  pluginSettings = builtins.listToAttrs (map (name: {
    inherit name;
    value = enabled name;
  }) all);

  formatters = if formatter == "none" then [] else [formatter];
}
