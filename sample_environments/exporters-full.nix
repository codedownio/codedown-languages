{ codedown
, ...
}:

codedown.makeEnvironment {
  exporters.nbconvert.enable = true;
  exporters.nbconvert.texliveScheme = "scheme-full";
}
