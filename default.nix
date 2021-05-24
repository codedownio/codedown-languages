{pkgs}:

with pkgs;
with pkgs.lib;

rec {
  # Languages
  cPack = callPackage ./languages/c {};
  clojurePack = callPackage ./languages/clojure {};
  # csharpPack = callPackage ./languages/csharp {};
  elixirPack = callPackage ./languages/elixir {};
  erlangPack = callPackage ./languages/erlang {};
  goPack = callPackage ./languages/go {};
  haskellPack = callPackage ./languages/haskell {};
  javascriptPack = callPackage ./languages/javascript {};
  schemePack = callPackage ./languages/scheme {};
  sqlPack = callPackage ./languages/sql {};

  # Languages
  languages = {
    bash = callPackage ./languages/bash {};
    dot = callPackage ./languages/dot {};
    cpp = callPackage ./languages/cpp {};
    julia = callPackage ./languages/julia {};
    octave = callPackage ./languages/octave {};
    python = callPackage ./languages/python {};
    r = callPackage ./languages/r {};
    ruby = callPackage ./languages/ruby {};
    rust = callPackage ./languages/rust {};
  };
  allBaseOptions = mapAttrs (name: value: value.metadata.baseOptions) languages;

  # Tools
  nixPackageManager = import ./package_managers/nix_package_manager;

  # Notebook language servers
  spellchecker = import ./language_servers/markdown-spellcheck-lsp.nix;

  # Tools
  zshWithTheme = import ./tools/zsh-with-theme;
  powerline = import ./tools/powerline;

  # Build tools
  mkCodeDownEnvironment = {
    spec ? null
    , specHash ? null
    , kernels
    , notebookLanguageServers
  }: symlinkJoin {
    name = "codedown-environment";
    paths = kernels ++ notebookLanguageServers;
    postBuild = ''
      cd $out
      mkdir -p lib

      specHash='${toString specHash}'
      if [[ -n "$specHash" ]]; then echo "$specHash" > lib/hash; fi

      spec='${toString spec}'
      if [[ -n "$spec" ]]; then echo "$spec" > lib/spec.yaml; fi
    '';
  };
}
