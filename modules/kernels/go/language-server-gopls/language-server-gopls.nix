{ lib
, callPackage
, runCommand
, makeWrapper
, pkgs
, system

, attrs
, kernelName
, settings

, go
, gopls
}:

let
  common = callPackage ../common.nix {};

  gnls = callPackage ./gnls.nix { inherit system; };

  gnlsVersion = import ./gnls-version.nix;

  go-parser = callPackage ./go-parser.nix {};

  gnls-wrapped = runCommand "go-notebook-language-server-${gnlsVersion}-wrapped" {
    nativeBuildInputs = [ makeWrapper ];
  } ''
    mkdir -p $out/bin
    makeWrapper ${gnls}/bin/go-notebook-language-server $out/bin/go-notebook-language-server \
      --prefix PATH : ${go-parser}/bin
  '';

  goplsWrapped = runCommand "gopls-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${gopls}/bin/gopls $out/bin/gopls \
      --suffix PATH ":" ${go}/bin \
      --set GOCACHE /tmp/gocache \
      --set GOPATH ${go}/share/go
  '';

  languageServerName = "gopls";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru gopls.meta passthru "lib/codedown/language-servers/go-${kernelName}-gopls.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = gopls.version;
  display_name = "gopls";
  description = gopls.meta.description;
  icon = ./go-logo-64x64.png;
  extensions = ["go"];
  notebook_suffix = ".go";
  kernel_name = kernelName;
  header_lines = ["package Notebook"];
  attrs = attrs;
  type = "stream";
  args = [
    "${gnls-wrapped}/bin/go-notebook-language-server"
    "--wrapped-server" "${goplsWrapped}/bin/gopls"
  ]
  ++ lib.optionals settings.debug ["--log-level" "debug"]
  ++ lib.optionals settings.super-debug ["--debug-client-writes" "--debug-client-reads" "--debug-server-writes" "--debug-server-reads"]
  ;
  env = {};
  language_id = "go";
}])
