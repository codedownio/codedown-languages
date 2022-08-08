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
    rev = "725376a278f2975b35d0ccbe28e58a62bd94cc2a";
    sha256 = "sha256-TGbrhZsU2wcBKjb1I1ic3UyKdNinIfxQXT0FN7JuUwI=";
  };

  hnls = (callPackage hnlsSrc {
    haskellNix = null;
    inherit pkgs;
  });

  exe = hnls.haskell-notebook-language-server.components.exes.haskell-notebook-language-server;

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
  args = [
    "${exe}/bin/haskell-notebook-language-server"
    "--wrapped-hls" "${haskell-language-server}/bin/haskell-language-server"
    "--hls-args" "--lsp"
  ];
  env = {};
}])
