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
    windowManager.session = with pkgs; [{
      manage = "window";
      name = "xsession";
      start = ". $HOME/.xsession";
    }];
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
