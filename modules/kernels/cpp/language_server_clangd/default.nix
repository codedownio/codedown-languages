{ lib
, callPackage
, runCommand
, makeWrapper
, llvmPackages
, system
, cling

, kernelName
, settings
}:

let
  common = callPackage ../../common.nix {};

  clangd = llvmPackages.clang-tools;

  cnls = callPackage ./cnls.nix { inherit system; };

  cnlsVersion = import ./cnls-version.nix;

  cling-parser = callPackage ./cling-parser.nix { inherit cling; };

  cnls-wrapped = runCommand "cpp-notebook-language-server-${cnlsVersion}-wrapped" {
    nativeBuildInputs = [ makeWrapper ];
  } ''
    mkdir -p $out/bin
    makeWrapper ${cnls}/bin/cpp-notebook-language-server $out/bin/cpp-notebook-language-server \
      --prefix PATH : ${cling-parser}/bin
  '';

  languageServerName = "clangd";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru clangd.meta passthru "lib/codedown/language-servers/cpp-${kernelName}-${languageServerName}.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = clangd.version;
  extensions = ["cpp" "hpp" "cxx" "hxx" "c" "h"];
  notebook_suffix = ".cpp";
  attrs = ["cpp"];
  type = "stream";
  primary = true;
  args = [
    "${cnls-wrapped}/bin/cpp-notebook-language-server"
    "--wrapped-server" "${clangd}/bin/clangd"
  ]
  ++ lib.optionals settings.debug ["--log-level" "debug"]
  ++ lib.optionals settings.super-debug ["--debug-client-writes" "--debug-client-reads" "--debug-server-writes" "--debug-server-reads"]
  ;
  language_id = "cpp";
}])
