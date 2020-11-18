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
    xorg.xinit velox
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
        extraConfig = ''
          [X11]
          UserAuthFile=.local/share/sddm/Xauthority
        '';
      };
      setupCommands = ''
        export XDG_RUNTIME_DIR=/run/user/$(id --user)
        export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id --user)/bus

        xrandr \
          --output DisplayPort-1 --mode 1920x1080 --pos 0x270 --auto \
          --output HDMI-A-1 --mode 1920x1080 --pos 1920x0 --scale 1.0x1.0 \
          --output HDMI-A-0 --mode 1920x1080 --pos 4320x270 --auto \
          --output DVI-D-0 --mode 1920x1080 --pos 6240x270 --auto
      ''; # Scale was at 1.25 previously
      autoLogin = {
        enable = true;
        user = "bao";
      };
     #defaultSession = "none+exwm";
      defaultSession = "none+openbox";
    };
    windowManager.openbox.enable = true;
    windowManager.awesome.enable = true;
  };
}
