{ lib
, callPackage

, rWrapper
, rPackages
, basePackages

, languageserver

, kernelName
}:

let
  common = callPackage ../../common.nix {};

  rWithPackagesAndLanguageServer = rWrapper.override {
    packages = [languageserver] ++ languageserver.languageServerDeps ++ basePackages;
  };

in

common.writeTextDirWithMeta languageserver.meta "lib/codedown/language-servers/r-${kernelName}-languageserver.yaml" (lib.generators.toYAML {} [{
  name = "languageserver";
  version = languageserver.version;
  display_name = "";
  description = "An implementation of the Language Server Protocol for R";
  icon = null;
  extensions = ["r"];
  notebook_suffix = ".r";
  attrs = ["r" "R"];
  type = "stream";
  primary = true;
  args = ["${rWithPackagesAndLanguageServer}/bin/R" "--slave" "-e" "languageserver::run()"];
}])
