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
    rev = "a1ad12e962f900a82100b59e205b613ecd456a71";
    sha256 = "15pm7c478nivbzwwl1r1yhgqac10yxxaaanfsp1v69xybkdkyxl3";
  };
  # hnlsSrc = /home/tom/tools/haskell-notebook-language-server;

  hnls = ghc.callPackage hnlsSrc {};

  exe = hnls;

  config = raw:
    {
      name = "haskell-language-server${if raw then "-raw" else ""}";
      display_name = "Haskell Language Server";
      description = haskell-language-server.meta.description;
      icon = ./icon_64x64.png;
      extensions = if raw then ["hs"] else [];
      notebook_suffix = if raw then ".hs" else "";
      kernel_name = kernelName;
      attrs = if raw then [] else ["haskell"];
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
    };

in

common.writeTextDirWithMeta haskell-language-server.meta "lib/codedown/language-servers/haskell-hls.yaml" (lib.generators.toYAML {} [
  (config false)
])
