{ config ? {}, lib, pkgs, ... }:

{
  imports = [
  ];

  # Enable the X11 windowing system.
  services.xserver = {
    windowManager.session = with pkgs; [{
      manage = "window";
      name = "xsession";
      start = ". $HOME/.xsession";
    }];
    displayManager = with pkgs; {
      defaultSession = "none+xsession";
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
