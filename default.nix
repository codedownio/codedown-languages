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
      (callPackage ./languages/bash {})
      (callPackage ./languages/clojure {})
      (callPackage ./languages/cpp {})
      (callPackage ./languages/dot {})
      (callPackage ./languages/julia {})
      (callPackage ./languages/octave {})
      (callPackage ./languages/python {})
      (callPackage ./languages/r {})
      (callPackage ./languages/ruby {})
      (callPackage ./languages/rust {})
    ];
    languagesSearcher = common.searcher languages;

    # Notebook language servers
    spellchecker = import ./language_servers/markdown-spellcheck-lsp.nix;

    # Shells
    availableShells = {
      zshWithTheme = common.wrapShell "zsh-with-theme" (callPackage ./tools/zsh-with-theme {});
      fish = common.wrapShell "fish" (callPackage ./shells/fish {});
      bash = common.wrapShell "bash" (prev.bashInteractive);
    };

    # Build tools
    mkCodeDownEnvironment = args@{
      channels
      , importedChannels
      , overlays
      , shells ? ["zshWithTheme"]
      , kernels ? []
      , otherPackages ? []
    }: let
      builtKernels = map (x: let kernel = (getAttr x.language (getAttr x.channel importedChannels).codedown.languages).build x.args; in
                             kernel.overrideAttrs (old: {
                               passthru = old.passthru // {
                                 language = x.language;
                                 channel = x.channel;
                               };
                             })) kernels;
      in
      symlinkJoin {
        name = "codedown-environment";
        paths = builtKernels
                ++ [(specYaml (args //  { kernels = builtKernels; }))]
                ++ [(common.wrapShells availableShells shells)]
                ++ (map (x: x.contents) otherPackages);
      };
  };

  specYaml = {
    channels
    , overlays
    , shells
    , kernels ? []
    , otherPackages ? []
    , ...
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

    inherit shells;

    kernels = map (x: {
      channel = x.channel;
      language = x.language;
      display_name = attrByPath ["meta" "displayName"] null x;
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
      settings_schema = attrByPath ["passthru" "settingsSchema"] null x;
      settings = attrByPath ["passthru" "settings"] null x;
    }) kernels;

    other_packages = map (x: {
      channel = x.channel;
      attr = x.attr;
      name = x.contents.name;
      meta = x.contents.meta;
    }) otherPackages;
  });
}
