{ lib
, fetchFromGitHub
, callPackage
, runCommand
, makeWrapper

, haskell-language-server

# Needed by haskell-language-server's wrapper, introduced in
# https://github.com/haskell/haskell-language-server/pull/2675
, coreutils
, findutils
, gnused

, kernelName
, ghc
}:

with lib;

let
  common = callPackage ../../common.nix {};

  hnlsSrc = fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell-notebook-language-server";
    rev = "4b1428f102326da120e10abb5477a24e456cb48a";
    sha256 = "a+YZo211VbkawjmouWukT6j0AxFwP+ThBQ+qoYrZS1I=";
  };
  # hnlsSrc = /home/tom/tools/haskell-notebook-language-server;

  hnls = ghc.callPackage hnlsSrc {};

  exe = hnls;

  hlsWrapped = runCommand "haskell-language-server-${haskell-language-server.version}-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${haskell-language-server}/bin/haskell-language-server $out/bin/haskell-language-server \
      --suffix PATH ':' ${lib.makeBinPath [coreutils findutils gnused]}
  '';

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
               "${hlsWrapped}/bin/haskell-language-server"
               "--lsp"
             ]
             else [
               "${exe}/bin/haskell-notebook-language-server"
               "--wrapped-hls" "${hlsWrapped}/bin/haskell-language-server"
               "--hls-args" "--lsp"
             ];
      env = {};
    };

in

common.writeTextDirWithMeta haskell-language-server.meta "lib/codedown/language-servers/haskell-hls.yaml" (lib.generators.toYAML {} [
  (config false)
])
