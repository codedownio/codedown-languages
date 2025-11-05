{ lib
, callPackage

, tinymist

# TODO: how to make the typstToUse (i.e. Typst with some packages) available to tinymist?
, typstToUse

, kernelName
}:

let
  common = callPackage ../../../kernels/common.nix {};

  languageServerName = "tinymist";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru tinymist.meta passthru "lib/codedown/language-servers/typst-${kernelName}-tinymist-language-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = tinymist.version;
  extensions = ["typ"];
  notebook_suffix = ".typ";
  attrs = ["typst"];
  type = "stream";
  primary = true;
  args = [
    "${tinymist}/bin/tinymist"
  ];
}])
