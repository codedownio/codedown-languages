{ callPackage
, fetchurl
, lib
, makeWrapper
, runCommand
, stdenv

, hunspell
, hunspellDicts
# , hunspellWithDicts

, nodehun
, nodejs-slim

, unixtools
}:

let
  common = callPackage ../../kernels/common.nix {};

  nodejs = nodejs-slim;

  # nodehun's native build fails on Darwin under newer Apple clang; patch it.
  nodehun' = if stdenv.isDarwin
             then (import ./nodehun-darwin-fix.nix) nodehun
             else nodehun;

  # nodeHeaders = runCommand "node-${nodejs.version}-headers.tar.gz" { buildInputs = []; } ''
  #   dir="node-v${nodejs.version}"
  #   mkdir "$dir"
  #   cp -r ${nodejs}/include "$dir"
  #   tar -czvf $out "$dir"
  # '';

  version = "0.6.3";

  indexJs = stdenv.mkDerivation {
    name = "markdown-spellcheck-lsp-index.js";

    src = fetchurl {
      url = "https://github.com/codedownio/markdown-spellcheck-lsp/releases/download/v${version}/markdown-spellcheck-lsp.tar.gz";
      hash = "sha256-99bGac3f15WO4AEG/n9UeoSTaHMjXf9xnKxuT434JnU=";
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
      --set NODE_PATH ${nodehun'}/lib/node_modules \
      --add-flags ${indexJs}
  '';

  meta = hunspell.meta // {
    icon = ./pen-alt.png;
    iconMonochrome = ./pen-alt.svg;
    displayName = "Spellchecker ${version}";
    category = "Language servers";
  };

in

(common.writeTextDirWithMeta meta "lib/codedown/language-servers/codedown-spellchecker.yaml" (lib.generators.toYAML {} [{
  name = "spellchecker";
  inherit version;
  extensions = ["md" "ipynb"];
  attrs = ["markdown"];
  type = "stream";
  icon = ./pen-alt.png;
  icon_monochrome = ./pen-alt.svg;
  notebook_suffix = ".spellchecker";
  args = [
    "${contents}/bin/markdown-spellcheck-lsp"
    "--affix-file" "${hunspellDicts.en-us}/share/hunspell/en_US.aff"
    "--dic-file" "${hunspellDicts.en-us}/share/hunspell/en_US.dic"
    "--personal-dic-file" ".codedown/personal-dictionary.dat"
    # "--log-level" "4"
    "--stdio"
  ];
}])).overrideAttrs (old: {
  passthru = {
    languageServerNames = ["spellchecker"];
  };
})
