{ stdenv
, pkgs
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  bashLanguageServer = (callPackage ./bash-language-server {
    nodejs = pkgs.nodejs-14_x;
  })."bash-language-server-2.0.0";

  # manWithPages = (import ../shared.nix).manWithPages;

  bashLanguageServerWithMan = runCommand "bash-language-server-with-man" {
    buildInputs = [makeWrapper];
    propagatedBuildInputs = [unixtools.col];
  } ''
      mkdir -p $out/bin
      makeWrapper ${bashLanguageServer}/bin/bash-language-server $out/bin/bash-language-server \
                  --suffix PATH ':' ${unixtools.col}/bin
    '';
in

common.writeTextDirWithMeta bashLanguageServer.meta "lib/codedown/bash-language-server-language-servers.yaml" (lib.generators.toYAML {} [{
  name = "bash";
  extensions = ["sh" "bash"];
  attrs = ["bash"];
  type = "stream";
  primary = true;
  args = [
    "${bashLanguageServerWithMan}/bin/bash-language-server"
    "start"
  ];
}])
