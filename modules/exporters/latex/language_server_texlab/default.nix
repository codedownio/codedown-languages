{ callPackage
, lib
, texlab

, texInputs
}:

let
  common = callPackage ../../../kernels/common.nix {};

  languageServerName = "texlab";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru texlab.meta passthru "lib/codedown/language-servers/latex-texlab-language-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = texlab.version;
  icon = ../../../../codedown.png;
  icon_monochrome = ../../../../codedown-monochrome.svg;
  extensions = ["tex"];
  notebook_suffix = ".tex";
  attrs = ["latex"];
  type = "stream";
  primary = true;
  args = [
    "${texlab}/bin/texlab"
  ];
  env = {
    TEXINPUTS = texInputs;
  };
}])
