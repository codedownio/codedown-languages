{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "exporters-latex";

  exporters.latex.enable = true;
}
