{ callPackage
, lib

, wrapper
, version
}:

let
  common = callPackage ../../../kernels/common.nix {};

  languageServerName = "pandoc-export-server";

  passthru = {
    inherit languageServerName;
  };

in

# The export server converts the in-memory Markdown document for live exports (see the
# codedown.render protocol in codedownio/lsp-export-server). It only handles its own commands.
common.writeTextDirWithMetaAndPassthru { description = "Live pandoc rendering for codedown exports"; } passthru "lib/codedown/language-servers/pandoc-export-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  inherit version;
  icon = ../../../../codedown.png;
  icon_monochrome = ../../../../codedown-monochrome.svg;
  extensions = ["md"];
  notebook_suffix = ".md";
  attrs = ["markdown-document"];
  type = "stream";
  primary = false;
  args = [
    "${wrapper}/bin/pandoc-export-server"
    "serve"
    "--backend" "pandoc"
    "-o" "engine=pdflatex"
  ];
}])
