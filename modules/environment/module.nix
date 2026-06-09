{ lib, ... }:

with lib;

{
  options = {
    # Commented out for now: environment.variables isn't wired up to anything at runtime yet (the
    # .env file it produced wasn't consumed), so it's hidden from the settings UI until it does.
    # environment.variables = mkOption {
    #   title = "Environment variables";
    #   type = types.attrsOf types.str;
    #   default = {};
    #   description = "Environment variables to set.";
    # };

    environment.extraNix = mkOption {
      title = "Extra Nix";
      type = types.codeMirrorLines "nix";
      default = "";
      example = ''
        with pkgs;
        symlinkJoin {
          name = "extra-packages";
          paths = [ hello cowsay ];
        }
      '';
      description = "Arbitrary Nix expression that evaluates to a derivation, which will be joined into the environment. The expression has \"pkgs\" in scope, an imported Nixpkgs package set.";
    };
  };

  config = {

  };
}
