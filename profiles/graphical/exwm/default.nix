{ config, lib, pkgs, ... }:

{
  services.xserver = {
    windowManager.exwm = {
      enable = true;
      enableDefaultConfig = false;
    };
    displayManager = with pkgs; {
      defaultSession = "none+exwm";
    };
  };
}
