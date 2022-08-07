{ lib
, pkgs
, haskell-language-server
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  hls = callPackage ./haskell-notebook-language-server {
    haskellNix = null;
    inherit pkgs;
  };

  hnlsSrc = fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell-notebook-language-server";
    rev = "55becca06f0c3a0a01773c2e61d6ebaa4896b9ec";
    sha256 = "sha256-0h/Co3mIu1cMBEYlPag18qxhq+n78LkqxC6Dk+3TqmQ=";
  };

  hnls = callPackage hnlsSrc {
    haskellNix = null;
    inherit pkgs;
  };

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
  args = ["${hnls}/bin/haskell-language-server" "--lsp"];
  env = {};
}])
