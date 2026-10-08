{ callPackage
, lib
, makeWrapper
, runCommand
, writeTextDir

, rubyPackages
, kernelName
, settings
}:

let
  common = callPackage ../common.nix {};

  languageServerName = "solargraph";

  solargraphRaw = rubyPackages.solargraph;

  solargraph = runCommand "solargraph-${solargraphRaw.version}-wrapped" {
    inherit (solargraphRaw) meta version;

    nativeBuildInputs = [makeWrapper];
  } ''
    mkdir -p $out/bin
    makeWrapper ${solargraphRaw}/bin/solargraph $out/bin/solargraph \
      --set XDG_CONFIG_HOME "${writeTextDir "rubocop/config.yml" settings.rubocopYaml}"
  '';

  passthru = {
    inherit languageServerName;
    formatters = lib.optional settings.formatting "rubocop";
  };

in

common.writeTextDirWithMetaAndPassthru solargraph.meta passthru "lib/codedown/language-servers/ruby-solargraph.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = solargraph.version;
  display_name = "Solargraph";
  description = "A Ruby language server";
  icon = ./iruby-64x64.png;
  icon_monochrome = ./ruby-monochrome.svg;
  extensions = ["rb"];
  notebook_suffix = ".rb";
  kernel_name = kernelName;
  attrs = ["ruby"];
  type = "stream";
  args = ["${solargraph}/bin/solargraph" "stdio"];
  # Solargraph only advertises documentFormattingProvider when this is on; it formats with
  # rubocop, which is already a dependency of the gem and reads the rubocopYaml above.
  initialization_options = {
    formatting = settings.formatting;
  };
  language_id = "ruby";
}])
