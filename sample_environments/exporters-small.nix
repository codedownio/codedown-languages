{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "exporters-small";

  exporters.nbconvert.enable = true;
}
