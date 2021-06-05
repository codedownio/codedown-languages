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
    languages = zipAttrsWith (n: v: head v) [
      (callPackage ./languages/python {})
      (callPackage ./languages/bash {})
      (callPackage ./languages/dot {})
      (callPackage ./languages/cpp {})
      (callPackage ./languages/julia {})
    ];
    languagesSearcher = common.searcher languages;

    # languages = {
    #   octave = callPackage ./languages/octave {};
    #   python = callPackage ./languages/python {};
    #   r = callPackage ./languages/r {};
    #   ruby = callPackage ./languages/ruby {};
    #   rust = callPackage ./languages/rust {};
    # };
    # allBaseOptions = listToAttrs (flatten (mapAttrsToList (name: value: map (x: {
    #   name = x.name;
    #   value = getAttrs ["name" "meta"] x;
    # }) value.metadata.baseOptions) languages));

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

    kernels = map (x: {
      channel_name = "nixpkgs"; # TODO
      language = x.passthru.args.baseName;
      base_name = x.passthru.args.baseName;
      display_name = attrByPath ["meta" "displayName"] null x.passthru;
      icon = attrByPath ["meta" "icon"] null x.passthru;
      meta = attrByPath ["meta"] null x.passthru;
      packages = map (name: {
        inherit name;
        meta = attrByPath [name "meta"] null x.passthru.packageOptions;
      }) x.passthru.args.packages;
      language_servers = map (name: {
        inherit name;
        meta = attrByPath [name "meta"] null x.passthru.languageServerOptions;
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
