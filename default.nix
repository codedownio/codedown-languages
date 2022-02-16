final: prev:

with final;
with final.lib;

let
  common = callPackage ./languages/common.nix {};

in

rec {
  "codedown.spellchecker" = callPackage ./language_servers/markdown-spellcheck-lsp.nix {};

  "codedown.exporters.slidy" = callPackage ./exporters/slidy.nix {};

  codedown = rec {
    nixpkgsSearcher = common.searcher final;

    shells = {
      zsh = let baseDerivation = callPackage ./tools/zsh-with-theme {}; in
            common.wrapShell "zsh-with-theme" baseDerivation ("ZSH " + baseDerivation.version) ./shells/default_icon.png;
      fish = let baseDerivation = callPackage ./shells/fish {}; in
             common.wrapShell "fish" baseDerivation ("Fish " + baseDerivation.version) ./shells/fish/icon-64x64.png;
      bash = common.wrapShell "bash" prev.bashInteractive ("Bash " + prev.bashInteractive.version) ./shells/default_icon.png;
    };
    availableShells = shells;
    shellsSearcher = common.searcher' "codedown.shells." shells;

    # Languages
    # First argument controls whether attributes get filtered to the valid ones.
    # This can be expensive to evaluate for languages like Haskell where there are tons of
    # Stackage snapshots and one nix file for each. So, we don't bother with that when evaluating
    # the languages attrset normally--only when building the languagesSearcher.
    languagesFn = filterToValid: zipAttrsWith (n: v: head v) [
      (callPackage ./languages/bash {})
      (callPackage ./languages/clojure {})
      (callPackage ./languages/cpp {})
      (callPackage ./languages/dot {})
      (callPackage ./languages/haskell { inherit filterToValid; })
      (callPackage ./languages/julia {})
      (callPackage ./languages/octave {})
      (callPackage ./languages/python {})
      (callPackage ./languages/r {})
      (callPackage ./languages/ruby {})
      (callPackage ./languages/rust {})
    ];
    languages = languagesFn false;

    languagesSearcher = common.searcher (languagesFn true);

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

      shellToReplInfo = shell: {
        name = shell.contents.name;
        display_name = shell.contents.displayName;
        args = ["${shell.contents}/lib/codedown/shell"];
        icon = shell.contents.icon;
      };

      repls =
        map shellToReplInfo shells
        ++ concatMap (kernel: lib.mapAttrsToList (name: value: value // { inherit name; }) (if kernel.passthru ? "repls" then kernel.passthru.repls else {})) builtKernels
        ;

      in
      symlinkJoin {
        name = "codedown-environment";
        paths = builtKernels
                ++ [(specYaml (args //  { kernels = builtKernels; }))]
                ++ (if metaOnly then [] else [(common.wrapShells shells)])
                ++ (if metaOnly then [] else (map (x: x.contents) otherPackages))
                ++ [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
        ;
      };
  };

  specYaml = {
    channels
    , overlays
    , shells
    , repls ? {}
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
