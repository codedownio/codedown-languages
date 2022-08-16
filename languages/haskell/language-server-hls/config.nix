{ lib
, pkgs
, haskell-language-server
, kernelName
, ghc
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

in

common.writeTextDirWithMeta haskell-language-server.meta "lib/codedown/haskell-hls-language-servers.yaml" (lib.generators.toYAML {} [{
  name = "haskell-language-server";
  display_name = "Haskell Language Server";
  description = haskell-language-server.meta.description;
  icon = ./icon_64x64.png;
  extensions = ["hs"];
  notebook_suffix = "";
  kernel_name = kernelName;
  attrs = ["haskell"];
  type = "stream";
  primary = true;
  args = [
    "${exe}/bin/haskell-notebook-language-server"
    "--wrapped-hls" "${haskell-language-server}/bin/haskell-language-server"
    "--hls-args" "--lsp"
  ];
  # args = ["${haskell-language-server}/bin/haskell-language-server" "--lsp"];
  env = {};
}])
