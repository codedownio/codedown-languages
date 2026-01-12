{ lib
, runCommand
, callPackage
, makeWrapper
, nodePackages

, shellcheck
, unixtools

, kernelName
}:

let
  common = callPackage ../../common.nix {};

  bashLanguageServer = nodePackages.bash-language-server;

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

  languageServerName = "bash-language-server";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru bashLanguageServerWithMan.meta passthru "lib/codedown/language-servers/bash-${kernelName}-bash-language-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = bashLanguageServer.version;
  extensions = ["sh" "bash"];
  notebook_suffix = ".bash";
  attrs = ["bash"];
  type = "stream";
  primary = true;
  args = [
    "${bashLanguageServerWithMan}/bin/bash-language-server"
    "start"
  ];
  language_id = "shellscript";
}])
