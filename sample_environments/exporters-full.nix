{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "exporters-full";

  exporters.nbconvert.enable = true;
  exporters.nbconvert.texliveScheme = "scheme-full";
}
