{ lib
, callPackage
, llvmPackages

, kernelName
}:

let
  common = callPackage ../../common.nix {};

  clangd = llvmPackages.clang-tools;

  languageServerName = "clangd";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru clangd.meta passthru "lib/codedown/language-servers/cpp-${kernelName}-clangd.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = clangd.version;
  extensions = ["cpp" "hpp" "cxx" "hxx" "c" "h"];
  notebook_suffix = ".cpp";
  attrs = ["cpp"];
  type = "stream";
  primary = true;
  args = [
    "${clangd}/bin/clangd"
  ];
  language_id = "cpp";
}])
