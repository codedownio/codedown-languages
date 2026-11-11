{ callPackage
, lib

, wrapper
, version
}:

let
  common = callPackage ../../../kernels/common.nix {};

  languageServerName = "typst-export-server";

  passthru = {
    inherit languageServerName;
  };

in

# The export server compiles the in-memory document for live exports (see the codedown.render
# protocol in codedownio/lsp-export-server). It only handles its own commands, so tinymist keeps the
# editing features.
common.writeTextDirWithMetaAndPassthru { description = "Live Typst rendering for codedown exports"; } passthru "lib/codedown/language-servers/typst-export-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  inherit version;
  icon = ../typst.png;
  icon_monochrome = ../typst.svg;
  extensions = ["typ"];
  notebook_suffix = ".typ";
  attrs = ["typst"];
  type = "stream";
  primary = false;
  args = [
    "${wrapper}/bin/typst-export-server"
    "serve"
  ];
}])
