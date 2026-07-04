{ callPackage
, fetchurl
, lib
, makeWrapper
, runCommand
, stdenv

, hunspell
, hunspellDicts

, nodehun
, nodejs-slim

, settings ? {}
, settingsSchema ? []
}:

let
  common = callPackage ../../kernels/common.nix {};

  nodejs = nodejs-slim;

  # nodehun's native build fails on Darwin under newer Apple clang; patch it.
  nodehun' = if stdenv.isDarwin
             then (import ./nodehun-darwin-fix.nix) nodehun
             else nodehun;

  version = "0.1.0";

  # The release tarball is the webpack bundle plus the vendored Typst parser wasm: index.js + wasm/. The
  # parser (official typst-syntax crate compiled to WebAssembly) is instantiated by hand from ./wasm next to
  # index.js, so nothing extra is needed on NODE_PATH beyond the native nodehun addon.
  bundle = stdenv.mkDerivation {
    name = "typst-spellcheck-lsp-bundle";

    src = fetchurl {
      url = "https://github.com/codedownio/typst-spellcheck-lsp/releases/download/v${version}/typst-spellcheck-lsp.tar.gz";
      hash = "sha256-1ozfEXeJxiqNmTOOOkeYgcGfeqGzMQCs+2PhhcQ7R34=";
    };

    buildPhase = "true";

    installPhase = ''
      mkdir -p $out
      cp -r . $out/
    '';

    dontFixup = true;
  };

  contents = runCommand "typst-spellcheck-lsp-wrapped" {
    buildInputs = [makeWrapper];
  } ''
    mkdir -p $out/bin
    makeWrapper ${nodejs}/bin/node $out/bin/typst-spellcheck-lsp \
      --set NODE_PATH "${nodehun'}/lib/node_modules" \
      --add-flags ${bundle}/index.js
  '';

  meta = hunspell.meta // {
    icon = ./pen-alt.png;
    iconMonochrome = ./pen-alt.svg;
    displayName = "Typst Spellchecker ${version}";
    category = "Language servers";
  };

in

(common.writeTextDirWithMeta meta "lib/codedown/language-servers/codedown-typst-spellchecker.yaml" (lib.generators.toYAML {} [{
  name = "typst-spellchecker";
  inherit version;
  extensions = ["typ"];
  attrs = ["typst"];
  type = "stream";
  icon = ./pen-alt.png;
  icon_monochrome = ./pen-alt.svg;
  notebook_suffix = ".spellchecker";
  args = [
    "${contents}/bin/typst-spellcheck-lsp"
    "--affix-file" "${hunspellDicts.en-us}/share/hunspell/en_US.aff"
    "--dic-file" "${hunspellDicts.en-us}/share/hunspell/en_US.dic"
    "--personal-dic-file" ".codedown/personal-dictionary.dat"
    # "--log-level" "4"
  ]
  ++ lib.optionals (settings.checkLineComments or false) ["--check-line-comments"]
  ++ lib.optionals (settings.checkBlockComments or false) ["--check-block-comments"]
  ++ ["--stdio"];
}])).overrideAttrs (old: {
  passthru = {
    languageServerNames = ["typst-spellchecker"];
    inherit settings settingsSchema;
  };
})
