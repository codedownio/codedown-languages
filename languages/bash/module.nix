{ config, lib, pkgs, ... }:

{
  options = {
    myOption = lib.mkOption {
      type = lib.types.str;
      default = "default value";
      description = "An example option";
    };
  };

  config = {
    myOption = "foo";
  };
}
