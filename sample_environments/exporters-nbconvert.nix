{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "exporters-nbconvert";

  exporters.nbconvert.enable = true;
  exporters.nbconvert.texliveScheme = "scheme-full";
}
