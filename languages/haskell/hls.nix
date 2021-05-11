{haskell}:

{
  meta = haskell.haskellPackages.haskell-language-server.meta;
  
  config = {
    name = "haskell";
    extensions = ["hs"];
    attrs = ["haskell"];
    type = "stream";
    args = [
      "${haskell.haskellPackages.haskell-language-server}/bin/haskell-language-server-wrapper"
      "--lsp"
      "-l"
      "/tmp/hls.log"
    ];
    notebook_suffix = ".hs";
  };
}
