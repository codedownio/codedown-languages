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
  })."markdown-spellcheck-lsp-git+https://github.com/codedownio/markdown-spellcheck-lsp.git#c7b08c83fca94a0abc2604199ea02dacc6e267aa";

  contents = runCommand "markdown-spellcheck-lsp-wrapped" {
    buildInputs = [makeWrapper];
    propagatedBuildInputs = [unixtools.col];
  } ''
    mkdir -p $out/bin
    makeWrapper ${server}/bin/markdown-spellcheck-lsp $out/bin/markdown-spellcheck-lsp \
      --set LANG en_US.UTF-8
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
    "--affix-file" "${hunspellDicts.en-us}/share/hunspell/en_US.aff"
    "--dic-file" "${hunspellDicts.en-us}/share/hunspell/en_US.dic"
    "--stdio"
  ];
}])
