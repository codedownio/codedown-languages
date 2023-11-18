{ lib
, callPackage
, fetchFromGitHub
, hunspell
, hunspellDicts
, hunspellWithDicts
, makeWrapper
, node2nix
, nodePackages
, nodejs-18_x
, nodejs-slim-18_x
, python3
, stdenv
, runCommand
, unixtools
}:

with lib.lists;

let
  common = callPackage ../../languages/common.nix {};

  nodejs = nodejs-slim-18_x;

  nodehunWithNix = stdenv.mkDerivation {
    name = "nodehun-with-nix";
    src = fetchFromGitHub {
      owner = "Wulf";
      repo = "nodehun";
      rev = "03c9dcf1fcd965031a68553ccaf6487d1fe87f79";
      sha256 = "13baqdxq8m1rvcqpdx5kwwk32xppwv9k29d2w55ash48akk3v1ij";
    };

    dontConfigure = true;
    dontFixup = true;

    doCheck = false;

    buildInputs = [node2nix];

    buildPhase = "node2nix -18 -l package-lock.json";

    installPhase = "cp -r ./. $out";
  };

  nodeHeaders = runCommand "node-${nodejs.version}-headers.tar.gz" { buildInputs = []; } ''
    dir="node-v${nodejs.version}"
    mkdir "$dir"
    cp -r ${nodejs}/include "$dir"
    tar -czvf $out "$dir"
  '';

  nodehun = (callPackage nodehunWithNix { nodejs = nodejs-18_x; }).package.override {
    preRebuild = ''
      npm run build -- --tarball ${nodeHeaders}
    '';

    buildInputs = [python3 nodePackages.node-gyp stdenv];

    disallowedReferences = [ nodejs ];

    postInstall = ''
      # Only keep the necessary parts of build/Release to reduce closure size
      cd $out/lib/node_modules/nodehun
      mv build build_old
      mkdir build
      cp -r build_old/Release build/
      rm -rf build_old
      rm -rf build/Release/.deps

      # Remove a development script to eliminate runtime dependency on node
      rm node_modules/node-addon-api/tools/conversion.js
    '';
  };

  indexJs = stdenv.mkDerivation {
    name = "markdown-spellcheck-lsp-index.js";

    src = fetchTarball {
      url = https://github.com/codedownio/markdown-spellcheck-lsp/releases/download/v0.5.0/markdown-spellcheck-lsp.tar.gz;
      sha256 = "sha256:020kvqcv38d2nxcj6wgi1wamnpfdwqzss4fm3w3svwcn5ki22psz";
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

in

common.writeTextDirWithMeta hunspell.meta "lib/codedown/language-servers/codedown-spellchecker.yaml" (lib.generators.toYAML {} [{
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
    "--stdio"
  ];
}])
