{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "exporters-small";

  exporters.nbconvert.enable = true;
  exporters.nbconvert.texliveScheme = "scheme-small";
}
