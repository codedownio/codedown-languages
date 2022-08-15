{ lib
, pkgs
, haskell-language-server
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  # This must be chosen to match haskellNix.sources.nixpkgs!
  # We do it ourselves because we want to use fetchFromGitHub instead of fetchTarball.
  nixpkgsSrc = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ce6aa13369b667ac2542593170993504932eb836";
    sha256 = "sha256-M6bJShji9AIDZ7Kh7CPwPBPb/T7RiVev2PAcOi4fxDQ=";
  };

  haskellNix = import (fetchFromGitHub {
    owner = "input-output-hk";
    repo = "haskell.nix";
    rev = "9709b2d05acb8b2d1451e5d7593756ca3a1be7d7";
    sha256 = "sha256-GkAdLMNFxfHtoQVswsf+imdBDhK/msqva7KCq0VUhNA=";
  }) { pkgs = import nixpkgsSrc { inherit system; }; };


  hls = callPackage ./haskell-notebook-language-server {
    haskellNix = null;
    inherit pkgs;
  };

  hnlsSrc = fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell-notebook-language-server";
    rev = "02e48a23368499333a6a9ae023eff50991637360";
    sha256 = "12vd8zqsvj64bw2srmcp2xy8952kysyy6v99y299bpaq84l1pwfh";
  };
  # hnlsSrc = /home/tom/tools/haskell-notebook-language-server;

  hnls = (callPackage hnlsSrc {
    haskellNix = null;
    pkgs = pkgs // {
      haskell-nix = (import nixpkgsSrc (haskellNix.nixpkgsArgs // { inherit system; })).haskell-nix;
    };
  });

  exe = hnls.haskell-notebook-language-server.components.exes.haskell-notebook-language-server;

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
