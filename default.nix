final: prev:

with final;
with final.lib;

let
  common = callPackage ./languages/common.nix {};

in

rec {
  codedown = rec {
    nixpkgsSearcher = common.searcher prev;

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
    allBaseOptions = listToAttrs (flatten (mapAttrsToList (name: value: map (x: {
      name = x.name;
      value = getAttrs ["name" "meta"] x;
    }) value.metadata.baseOptions) languages));

    languagesSearcher = common.searcher allBaseOptions;

    # Notebook language servers
    spellchecker = import ./language_servers/markdown-spellcheck-lsp.nix;

    # Tools
    zshWithTheme = callPackage ./tools/zsh-with-theme {};
    powerline = callPackage ./tools/powerline {};

    # Build tools
    mkCodeDownEnvironment = args@{
      channels
      , overlays
      , kernels ? []
      , otherPackages ? []
    }: symlinkJoin {
      name = "codedown-environment";
      paths = kernels ++ [(specYaml args)] ++ (map (x: x.contents) otherPackages);
    };
  };

  specYaml = {
    channels
    , overlays
    , kernels ? []
    , otherPackages ? []
  }: writeTextDir "lib/codedown/spec.yaml" (lib.generators.toYAML {} {
    channels = mapAttrsToList (name: value: {
      tag = "fetch_git";
      name = name;
      url = value.url;
      rev = value.rev;
      branchName = attrByPath ["branchName"] null value;
      sha256 = value.outputHash;
    }) channels;

    overlays = mapAttrsToList (name: value: if builtins.typeOf value == "path" then
      {
        tag = "path";
        name = name;
        path = value;
      } else {
        tag = "fetch_git";
        name = name;
        url = value.url;
        rev = value.rev;
        branch_name = attrByPath ["branchName"] null value;
        sha256 = value.outputHash;
      }) overlays;

    kernels = map (x:
      let
        base = x.metadata.baseByName x.passthru.args.baseName;
        packageOptions = x.passthru.metadata.packageOptions base;
        languageServerOptions = x.passthru.metadata.languageServerOptions base packageOptions;
      in
        {
          channel_name = "nixpkgs"; # TODO
          language = x.metadata.language;
          base_name = x.passthru.args.baseName;
          display_name = attrByPath ["meta" "displayName"] null x.passthru;
          icon = attrByPath ["meta" "icon"] null x.passthru;
          meta = attrByPath ["meta"] null x.passthru;
          packages = map (name: {
            inherit name;
            meta = attrByPath [name "meta"] null packageOptions;
          }) x.passthru.args.packages;
          language_servers = map (name: {
            inherit name;
            meta = attrByPath [name "meta"] null languageServerOptions;
          }) x.passthru.args.languageServers;
        }) kernels;

    other_packages = map (x: {
      channel = x.channel;
      attr = x.attr;
      name = x.contents.name;
      meta = x.contents.meta;
    }) otherPackages;
  });
}
