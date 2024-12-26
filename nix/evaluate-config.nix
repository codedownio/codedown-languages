{ lib
, pkgsStable
, pkgsMaster
}:

config:

lib.evalModules {
  specialArgs = {
    nixosOptionsToSettingsSchema = pkgsStable.callPackage ./nixos-options-to-settings-schema.nix {};
    boilerplate = {
      attrsTitle = "Notebook attributes";
      attrsDescription = "Notebook cells that have these attributes will match this kernel, allowing it to run the code.";

      extensionsTitle = "File extensions";
      extensionsDescription = "Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell.";
    };
  };
  modules = [
    ../modules/base.nix

    ../modules/environment/module.nix

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
