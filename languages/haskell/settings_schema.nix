{ lib
, version
}:

lib.optionals (builtins.compareVersions version "9.0" >= 0) [
  {
    target = "lsp.haskell-language-server.enable";
    title = "Enable haskell-language-server";
    type = "boolean";
    defaultValue = true;
  }
  {
    target = "lsp.haskell-language-server.debug";
    title = "Haskell-language-server: enable debug output";
    description = "Print debug output for the notebook language server.";
    type = "boolean";
    defaultValue = false;
  }
  {
    target = "lsp.haskell-language-server.super-debug";
    title = "Haskell-language-server: enable verbose debug output";
    description = "Print verbose debug output; intended for developers.";
    type = "boolean";
    defaultValue = false;
  }
]
++ [
  {
    target = "enableHlintOutput";
    title = "Enable hlint warnings in code output.";
    description = "Show hlint warnings as part of Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server.";
    type = "boolean";
    defaultValue = false;
  }
]
