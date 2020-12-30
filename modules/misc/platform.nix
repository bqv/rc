{ config, lib, options, ... }:

{
  options = {
    platform = lib.mkOption {
      type = lib.types.str;
      description = "Target platform";
    };
  };
}
