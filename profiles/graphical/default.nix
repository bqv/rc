{ config, lib, pkgs, ... }:

{
  imports = [
    ./xkb
    ./exwm
   #./xmonad
    ./lxqt
   #./qutebrowser
    ../develop
  ];

  environment.systemPackages = with pkgs; [
    velox
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
        autoLogin.relogin = true;
        extraConfig = ''
          [X11]
          UserAuthFile=.local/share/sddm/Xauthority
        '';
      };
      autoLogin = {
        enable = true;
        user = "bao";
      };
      defaultSession = "none+exwm";
    };
  };
}
