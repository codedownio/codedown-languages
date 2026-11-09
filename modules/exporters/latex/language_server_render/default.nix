{ callPackage
, lib

, wrapper
, version
, engine
}:

let
  common = callPackage ../../../kernels/common.nix {};

  languageServerName = "latex-render-server";

  passthru = {
    inherit languageServerName;
  };

in

# The render server compiles the in-memory document for live exports (see the codedown.render
# protocol in codedownio/render-server). It only handles its own commands, so texlab keeps the
# editing features.
common.writeTextDirWithMetaAndPassthru { description = "Live LaTeX rendering for codedown exports"; } passthru "lib/codedown/language-servers/latex-render-server.yaml" (lib.generators.toYAML {} [{
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
    "${wrapper}/bin/latex-render-server"
    "serve"
    "--backend" "latex"
    "-o" "engine=${engine}"
  ];
}])
