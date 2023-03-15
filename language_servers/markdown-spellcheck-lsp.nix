{ lib
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
  common = callPackage ../languages/common.nix {};

  server = (callPackage ./markdown-spellcheck-lsp {
    nodejs = nodejs-14_x;
  })."markdown-spellcheck-lsp-git+https://github.com/codedownio/markdown-spellcheck-lsp.git#d6832cf474bc382bc735a452a071ed2bf6e1828b";

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
                --prefix PATH ':' ${customHunspell}/bin
  '';

in

common.writeTextDirWithMeta hunspell.meta "lib/codedown/language-servers/codedown-spellchecker.yaml" (lib.generators.toYAML {} [{
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
}])
