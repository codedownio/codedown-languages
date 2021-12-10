final: prev:

with final;
with final.lib;

let
  common = callPackage ./languages/common.nix {};

in

rec {
  # Notebook language servers
  codedownSpellchecker = callPackage ./language_servers/markdown-spellcheck-lsp.nix {};

  codedown = rec {
    nixpkgsSearcher = common.searcher final;

    shells = {
      zsh = common.wrapShell "zsh-with-theme" (callPackage ./tools/zsh-with-theme {});
      fish = common.wrapShell "fish" (callPackage ./shells/fish {});
      bash = common.wrapShell "bash" prev.bashInteractive;
    };
    availableShells = shells;
    shellsSearcher = common.searcher' "codedown.shells." shells;

    # Languages
    languages = zipAttrsWith (n: v: head v) [
      (callPackage ./languages/bash {})
      (callPackage ./languages/clojure {})
      (callPackage ./languages/cpp {})
      (callPackage ./languages/dot {})
      (callPackage ./languages/haskell {})
      (callPackage ./languages/julia {})
      (callPackage ./languages/octave {})
      (callPackage ./languages/python {})
      (callPackage ./languages/r {})
      (callPackage ./languages/ruby {})
      (callPackage ./languages/rust {})
    ];
    languagesSearcher = common.searcher languages;

    # Build tools
    mkCodeDownEnvironment = args@{
      channels
      , importedChannels
      , overlays
      , shells ? []
      , kernels ? []
      , otherPackages ? []
      , metaOnly ? false
    }: let
      builtKernels = map (x: let kernel = (getAttr x.language (getAttr x.channel importedChannels).codedown.languages).build (x.args // { inherit metaOnly; }); in
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
                ++ (if metaOnly then [] else [(common.wrapShells shells)])
                ++ (if metaOnly then [] else (map (x: x.contents) otherPackages))
        ;
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
    channels = lib.mapAttrsToList (name: value: value // { inherit name; }) channels;

    overlays = lib.mapAttrsToList (name: value: value // { inherit name; }) overlays;

    shells = map (x: {
      channel = x.channel;
      attr = x.attr;
      name = x.contents.name;
      meta = x.contents.meta;
    }) shells;

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
