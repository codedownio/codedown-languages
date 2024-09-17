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

    ../exporters/module.nix

    ../languages/bash/module.nix
    ../languages/clojure/module.nix
    ../languages/coq/module.nix
    ../languages/cpp/module.nix
    ../languages/go/module.nix
    ../languages/haskell/module.nix
    ../languages/julia/module.nix
    ../languages/octave/module.nix
    ../languages/postgres/module.nix
    ../languages/python/module.nix
    ../languages/r/module.nix
    ../languages/ruby/module.nix
    ../languages/rust/module.nix

    ../language_servers/markdown-spellcheck-lsp/module.nix

    ../shells/bash/module.nix
    ../shells/fish/module.nix
    ../shells/zsh/module.nix

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
