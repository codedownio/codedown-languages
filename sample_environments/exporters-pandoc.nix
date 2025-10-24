{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "exporters-pandoc";

  exporters.pandoc.enable = true;
}
