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
    rev = "687f4d83fd2585a081499194d0bc1393153de5a5";
    sha256 = "1cihypr8pbi20ibbdqcj3apzbqgcnfrp2vch4472r43l0hvk7gcx";
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
