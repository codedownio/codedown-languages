{ lib
, pkgs
, haskell-language-server
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

in

common.writeTextDirWithMeta haskell-language-server.meta "lib/codedown/haskell-hls-language-servers.yaml" (lib.generators.toYAML {} [{
  name = "haskell-language-server";
  display_name = "Haskell Language Server";
  description = haskell-language-server.meta.description;
  icon = ./icon_64x64.png;
  extensions = ["hs"];
  notebook_suffix = ".hs";
  kernel_name = kernelName;
  attrs = ["haskell"];
  type = "stream";
  primary = true;
  args = ["${haskell-language-server}/bin/haskell-language-server"];
  env = {};
}])
