{ config, lib, pkgs, ... }:

{
  imports = [
    ./xkb
    ./exwm
    ./xmonad
    ./lxqt
   #./qutebrowser
    ../develop
  ];

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.pulseaudio.enable = lib.mkDefault true;

  services.xserver = {
    enable = true;

    displayManager = {
      sddm = {
        enable = true;
        theme = "chili";
        autoLogin = {
          enable = true;
          relogin = true;
          user = "bao";
        };
        extraConfig = ''
          [X11]
          UserAuthFile=.local/share/sddm/Xauthority
        '';
      };
      defaultSession = "none+xmonad";
    };
  };
}
