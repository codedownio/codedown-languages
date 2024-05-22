{ callPackage
, fetchFromGitHub
, lib
, makeWrapper
, runCommand
, stdenv

, hunspell
, hunspellDicts
, hunspellWithDicts

, node2nix
, nodePackages
, nodehun
, nodejs-18_x
, nodejs-slim-18_x

, python3
, unixtools
}:

with lib.lists;

let
  common = callPackage ../../languages/common.nix {};

  nodejs = nodejs-slim-18_x;

  nodeHeaders = runCommand "node-${nodejs.version}-headers.tar.gz" { buildInputs = []; } ''
    dir="node-v${nodejs.version}"
    mkdir "$dir"
    cp -r ${nodejs}/include "$dir"
    tar -czvf $out "$dir"
  '';

  indexJs = stdenv.mkDerivation {
    name = "markdown-spellcheck-lsp-index.js";

    src = fetchTarball {
      url = https://github.com/codedownio/markdown-spellcheck-lsp/releases/download/v0.6.1/markdown-spellcheck-lsp.tar.gz;
      sha256 = "sha256:1w7g3v01ziv98xanibfqb0xwkbl918mdrq4y0ryga8h7kfvifmc4";
    };

    buildPhase = "true";

    installPhase = ''
      echo 'var nodehun = require("nodehun");' > $out
      cat index.js >> $out
    '';

    dontFixup = true;
  };

  contents = runCommand "markdown-spellcheck-lsp-wrapped" {
    buildInputs = [makeWrapper];
    propagatedBuildInputs = [unixtools.col];
  } ''
    mkdir -p $out/bin
    makeWrapper ${nodejs}/bin/node $out/bin/markdown-spellcheck-lsp \
      --set NODE_PATH ${nodehun}/lib/node_modules \
      --add-flags ${indexJs}
  '';

  meta = hunspell.meta // {
    icon = ./pen-alt.png;
    category = "Language servers";
  };

in

common.writeTextDirWithMeta meta "lib/codedown/language-servers/codedown-spellchecker.yaml" (lib.generators.toYAML {} [{
  name = "spellchecker";
  version = "1.1.0";
  extensions = ["md" "ipynb"];
  attrs = ["markdown"];
  type = "stream";
  icon = ./pen-alt.png;
  notebook_suffix = ".spellchecker";
  args = [
    "${contents}/bin/markdown-spellcheck-lsp"
    "--affix-file" "${hunspellDicts.en-us}/share/hunspell/en_US.aff"
    "--dic-file" "${hunspellDicts.en-us}/share/hunspell/en_US.dic"
    "--personal-dic-file" ".codedown/personal-dictionary.dat"
    # "--log-level" "4"
    "--stdio"
  ];
}])
