{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    dmenu xmobar taffybar
  ];
  services.xserver = {
    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
    };
    displayManager = with pkgs; {
      defaultSession = "none+xmonad";
    };
  };
}
