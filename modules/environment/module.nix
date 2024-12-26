{ lib, ... }:

with lib;

{
  options = {
    environment.variables = mkOption {
      type = types.attrsOf types.string;
      default = {};
      description = "Environment variables to set.";
    };
  };

  config = {

  };
}
