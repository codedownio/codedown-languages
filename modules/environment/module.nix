{ lib, ... }:

with lib;

{
  options = {
    environment.variables = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = "Environment variables to set.";
    };
  };

  config = {

  };
}
