{ lib
, pkgsStable
, pkgsMaster
}:

config:

lib.evalModules {
  specialArgs = {
    nixosOptionsToSettingsSchema = pkgsStable.callPackage ../modules/base/nixos-options-to-settings-schema.nix {};
  };
  modules = [
    ../modules/base.nix

    ../modules/exporters/module.nix

    ../modules/languages/bash/module.nix
    ../modules/languages/clojure/module.nix
    ../modules/languages/coq/module.nix
    ../modules/languages/cpp/module.nix
    ../modules/languages/go/module.nix
    ../modules/languages/haskell/module.nix
    ../modules/languages/julia/module.nix
    ../modules/languages/octave/module.nix
    ../modules/languages/postgres/module.nix
    ../modules/languages/python/module.nix
    ../modules/languages/r/module.nix
    ../modules/languages/ruby/module.nix
    ../modules/languages/rust/module.nix

    ../modules/language_servers/markdown-spellcheck-lsp/module.nix

    ../modules/shells/bash/module.nix
    ../modules/shells/fish/module.nix
    ../modules/shells/zsh/module.nix

    {
      config = {
        pkgs = pkgsStable;
        inherit pkgsMaster;
      };
    }
    {
      inherit config;
    }
  ];
}
