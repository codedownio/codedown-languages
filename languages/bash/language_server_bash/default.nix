{ lib
, runCommand
, callPackage
, makeWrapper

, shellcheck
, unixtools

, kernelName
}:

with lib;

let
  common = callPackage ../../common.nix {};

  bashLanguageServer = (callPackage ./bash-language-server {})."bash-language-server-3.1.1";

  # manWithPages = (import ../shared.nix).manWithPages;

  bashLanguageServerWithMan = runCommand "bash-language-server" {
    buildInputs = [makeWrapper];
    propagatedBuildInputs = [unixtools.col];
  } ''
      mkdir -p $out/bin
      makeWrapper ${bashLanguageServer}/bin/bash-language-server $out/bin/bash-language-server \
                  --suffix PATH ':' ${unixtools.col}/bin \
                  --suffix PATH ':' ${shellcheck}/bin \
                  --set SHELLCHECK_PATH ${shellcheck}/bin/shellcheck
    '';
in

common.writeTextDirWithMeta bashLanguageServerWithMan.meta "lib/codedown/language-servers/bash-${kernelName}-bash-language-server.yaml" (lib.generators.toYAML {} [{
  name = "bash-language-server";
  extensions = ["sh" "bash"];
  notebook_suffix = ".bash";
  attrs = ["bash"];
  type = "stream";
  primary = true;
  args = [
    "${bashLanguageServerWithMan}/bin/bash-language-server"
    "start"
  ];
}])
