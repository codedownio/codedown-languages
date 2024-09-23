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

    ../modules/kernels/bash/module.nix
    ../modules/kernels/clojure/module.nix
    ../modules/kernels/coq/module.nix
    ../modules/kernels/cpp/module.nix
    ../modules/kernels/go/module.nix
    ../modules/kernels/haskell/module.nix
    ../modules/kernels/julia/module.nix
    ../modules/kernels/octave/module.nix
    ../modules/kernels/postgres/module.nix
    ../modules/kernels/python/module.nix
    ../modules/kernels/r/module.nix
    ../modules/kernels/ruby/module.nix
    ../modules/kernels/rust/module.nix

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
