{ lib
, callPackage
, llvmPackages
, system

, kernelName
}:

let
  common = callPackage ../../common.nix {};

  clangd = llvmPackages.clang-tools;

  cnls = callPackage ./cnls.nix { inherit system; };

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
    "${cnls}/bin/cpp-notebook-language-server"
    "--wrapped-clangd" "${clangd}/bin/clangd"
  ];
  language_id = "cpp";
}])
