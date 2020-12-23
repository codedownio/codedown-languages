{stdenv, pkgs}:

with pkgs;
with stdenv.lib;

let
  bashLanguageServer = (callPackage ../bash-language-server {})."bash-language-server-1.16.1";

  manWithPages = (import ../shared.nix).manWithPages;

  bashLanguageServerWithMan = runCommand "bash-language-server-with-man" {
    buildInputs = [makeWrapper];
    propagatedBuildInputs = [unixtools.col];
  } ''
      mkdir -p $out/bin
      makeWrapper ${bashLanguageServer}/bin/bash-language-server $out/bin/bash-language-server \
                  --suffix PATH ':' ${manWithPages}/bin \
                  --suffix PATH ':' ${unixtools.col}/bin
    '';
in

{
  config = {
    name = "bash";
    extensions = ["sh" "bash"];
    attrs = ["bash"];
    type = "stream";
    primary = true;
    args = [
      "${bashLanguageServerWithMan}/bin/bash-language-server"
      "start"
    ];
  };
}
