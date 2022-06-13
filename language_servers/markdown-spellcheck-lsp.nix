{ lib
, writeText
, callPackage
, hunspell
, hunspellDicts
, hunspellWithDicts
, runCommand
, makeWrapper
, unixtools
, nodejs-14_x
}:
with lib.lists;

let
  config = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "spellchecker";
    extensions = ["md" "ipynb"];
    attrs = ["markdown"];
    type = "stream";
    icon = ./pen-alt.svg;
    notebook_suffix = ".spellchecker";
    args = [
      "${contents}/bin/markdown-spellcheck-lsp"
      "--stdio"
    ];
  }]);

  server = (callPackage ./markdown-spellcheck-lsp {
    nodejs = nodejs-14_x;
  })."markdown-spellcheck-lsp-git+https://github.com/codedownio/markdown-spellcheck-lsp.git#f047c58b51ab93bbec54120c508728acc02c82c9";

  customHunspell = hunspellWithDicts [
    hunspellDicts.en-us
  ];

  contents = runCommand "markdown-spellcheck-lsp-wrapped" {
    buildInputs = [makeWrapper];
    propagatedBuildInputs = [unixtools.col];
  } ''
    mkdir -p $out/bin
    makeWrapper ${server}/bin/markdown-spellcheck-lsp $out/bin/markdown-spellcheck-lsp \
                --set LANG en_US.UTF-8 \
                --suffix PATH ':' ${customHunspell}/bin
  '';

in

runCommand "codedown-spellchecker" {
  meta = hunspell.meta;
} ''
  mkdir -p $out/lib/codedown
  cp ${config} $out/lib/codedown/hunspell-spellchecker-language-servers.yaml
''
