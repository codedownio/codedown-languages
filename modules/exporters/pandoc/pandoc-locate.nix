{ callPackage
, pandoc
}:

let
  common = callPackage ../../kernels/common.nix {};

in

# The locate program for pandoc-export-server (RENDER_QUERY in codedownio/lsp-export-server)
common.writeShellScriptBinWithAttrs {} "pandoc-locate" ''
  exec ${pandoc}/bin/pandoc lua ${./pandoc-locate.lua}
''
