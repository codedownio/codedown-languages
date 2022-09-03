{ lib
, pkgs
, haskell-language-server
, kernelName
, ghc
, raw
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  hnlsSrc = fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell-notebook-language-server";
    rev = "4dfe247f8c7f966b0a38fe01160f47db61124f33";
    sha256 = "0cy8kiszbpqvwbw3bqfx2ckpm2y4kizywbxdmhm6wd4j3ip31ibf";
  };
  # hnlsSrc = /home/tom/tools/haskell-notebook-language-server;

  hnls = ghc.callPackage hnlsSrc {};

  exe = hnls;

  suffix = if raw then "-raw" else "";

in

common.writeTextDirWithMeta haskell-language-server.meta "lib/codedown/language-servers/haskell-hls${suffix}.yaml" (lib.generators.toYAML {} [{
  name = "haskell-language-server";
  display_name = "Haskell Language Server";
  description = haskell-language-server.meta.description;
  icon = ./icon_64x64.png;
  extensions = if raw then ["hs"] else [];
  notebook_suffix = if raw then ".hs" else "";
  kernel_name = kernelName;
  attrs = ["haskell"];
  type = "stream";
  primary = true;
  args = if raw
         then [
           "${haskell-language-server}/bin/haskell-language-server"
           "--lsp"
         ]
         else [
           "${exe}/bin/haskell-notebook-language-server"
           "--wrapped-hls" "${haskell-language-server}/bin/haskell-language-server"
           "--hls-args" "--lsp"
         ];
  env = {};
}])
