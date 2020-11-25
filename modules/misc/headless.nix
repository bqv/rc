{ config, lib, options, ... }:

{
  options = {
    headless = lib.mkOption {
      type = lib.types.bool;
      description = "Is a headless machine";
    };
  };
}
