{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    dmenu xmobar taffybar
  ];
  services.xserver = {
    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
      xmonad.extraPackages = hpkgs: [
        hpkgs.taffybar
      ];
    };
    displayManager = with pkgs; {
      defaultSession = "none+xmonad";
    };
  };
}
