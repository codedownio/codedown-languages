{ lib
, runCommand
, callPackage
, makeWrapper

, nodejs-14_x
, shellcheck
, unixtools
}:

with lib;

let
  common = callPackage ../../common.nix {};

  bashLanguageServer = (callPackage ./bash-language-server {
    nodejs = nodejs-14_x;
  })."bash-language-server-3.1.0";

  # manWithPages = (import ../shared.nix).manWithPages;

  bashLanguageServerWithMan = runCommand "bash-language-server-with-man" {
    buildInputs = [makeWrapper];
    propagatedBuildInputs = [unixtools.col];
  } ''
      mkdir -p $out/bin
      makeWrapper ${bashLanguageServer}/bin/bash-language-server $out/bin/bash-language-server \
                  --suffix PATH ':' ${unixtools.col}/bin \
                  --set SHELLCHECK_PATH ${shellcheck}/bin/shellcheck
    '';
in

common.writeTextDirWithMeta bashLanguageServerWithMan.meta "lib/codedown/language-servers/bash-language-server.yaml" (lib.generators.toYAML {} [{
  name = "bash-language-server";
  extensions = ["sh" "bash"];
  notebook_suffix = ".sh";
  attrs = ["bash"];
  type = "stream";
  primary = true;
  args = [
    "${bashLanguageServerWithMan}/bin/bash-language-server"
    "start"
  ];
}])
