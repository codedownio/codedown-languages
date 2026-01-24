{ callPackage
, lib
, makeWrapper
, runCommand
, writeTextDir

, rubyPackages
, rubocopYaml
, kernelName
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
      --set XDG_CONFIG_HOME "${writeTextDir "rubocop/config.yml" rubocopYaml}"
  '';

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru solargraph.meta passthru "lib/codedown/language-servers/ruby-solargraph.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = solargraph.version;
  display_name = "Solargraph";
  description = "A Ruby language server";
  icon = ./iruby-64x64.png;
  extensions = ["rb"];
  notebook_suffix = ".rb";
  kernel_name = kernelName;
  attrs = ["ruby"];
  type = "stream";
  args = ["${solargraph}/bin/solargraph" "stdio"];
  language_id = "ruby";
}])
