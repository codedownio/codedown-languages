{ lib
, callPackage

, tinymist

# TODO: how to make the typstToUse (i.e. Typst with some packages) available to tinymist?
# deadnix: skip
, typstToUse

# The CodeDown Typst prelude package dir (see ../default.nix). tinymist's LSP has no --package-path
# CLI flag, so we hand it the path through initializationOptions.typstExtraArgs instead — verified to
# make @local/codedown resolve in tinymist's live preview.
, codedownPackagePath

, kernelName
, settings
}:

let
  common = callPackage ../../../kernels/common.nix {};

  languageServerName = "tinymist";

  passthru = {
    inherit languageServerName;
    formatters = lib.optional (settings.formatter != "none") settings.formatter;
  };

in

common.writeTextDirWithMetaAndPassthru tinymist.meta passthru "lib/codedown/language-servers/typst-${kernelName}-tinymist-language-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = tinymist.version;
  icon = ../typst.png;
  icon_monochrome = ../typst.svg;
  extensions = ["typ"];
  notebook_suffix = ".typ";
  attrs = ["typst"];
  type = "stream";
  primary = true;
  args = [
    "${tinymist}/bin/tinymist"
  ];
  initialization_options = {
    typstExtraArgs = [ "--package-path=${codedownPackagePath}" ];
    # tinymist has both typstyle and typstfmt vendored and defaults to typstyle, so this only
    # picks between them (or turns formatting off). Note that tinymist advertises
    # documentFormattingProvider either way and just answers null when it's off.
    formatterMode = if settings.formatter == "none" then "disable" else settings.formatter;
  };
}])
