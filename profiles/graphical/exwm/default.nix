{ config ? {}, lib, pkgs, ... }:

{
  imports = [
  ];

  # Enable the X11 windowing system.
  services.xserver = {
    windowManager.exwm = {
      enable = true;
      enableDefaultConfig = false;
    };
    displayManager = with pkgs; {
      defaultSession = "none+exwm";
      sddm = {
        enable = true;
        autoLogin = {
          enable = true;
          relogin = true;
          user = "bao";
        };
      };
    };
  };
}
