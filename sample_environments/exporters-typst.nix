{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "exporters-typst";

  exporters.typst.enable = true;
  exporters.typst.packages = [
    "aero-check"
  ];
}
