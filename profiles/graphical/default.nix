{ config, pkgs, ... }:

{
  imports = [
    ./exwm
    #./qutebrowser
    ../develop
  ];

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;

    displayManager.sddm = {
      enable = true;
      theme = "chili";
      autoLogin = {
        enable = true;
        relogin = true;
        user = "bao";
      };
    };
  };
}
