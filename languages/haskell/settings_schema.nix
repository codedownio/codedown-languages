[
  {
    target = "lsp.haskell-language-server.enable";
    title = "Enable haskell-language-server";
    type = "boolean";
    defaultValue = true;
  }
  {
    target = "lsp.haskell-language-server.debug";
    title = "Haskell-language-server: enable debug output";
    description = "Print verbose debug output.";
    type = "boolean";
    defaultValue = false;
  }

  {
    target = "enableHlintOutput";
    title = "Enable hlint warnings in code output.";
    description = "Show hlint warnings as part of Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server.";
    type = "boolean";
    defaultValue = false;
  }
]
