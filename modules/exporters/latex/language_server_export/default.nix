{ callPackage
, lib

, wrapper
, version
, engine
}:

let
  common = callPackage ../../../kernels/common.nix {};

  languageServerName = "latex-export-server";

  passthru = {
    inherit languageServerName;
  };

in

# The export server compiles the in-memory document for live exports (see the codedown.render
# protocol in codedownio/lsp-export-server). It only handles its own commands, so texlab keeps the
# editing features.
common.writeTextDirWithMetaAndPassthru { description = "Live LaTeX rendering for codedown exports"; } passthru "lib/codedown/language-servers/latex-export-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  inherit version;
  icon = ../../../../codedown.png;
  icon_monochrome = ../../../../codedown-monochrome.svg;
  extensions = ["tex"];
  notebook_suffix = ".tex";
  attrs = ["latex"];
  type = "stream";
  primary = false;
  args = [
    "${wrapper}/bin/latex-export-server"
    "serve"
    "--backend" "latex"
    "-o" "engine=${engine}"
  ];
}])
