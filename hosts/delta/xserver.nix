{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ miraclecast ];

  services.xserver = {
    enable = true;

    # Enable touchpad support.
    libinput.enable = true;

    layout = "gb";
    xkbModel = "pc105";
    xkbOptions = "compose:ralt,keypad:pointerkeys:ctrl:nocaps";

    serverLayoutSection = ''
      Option        "StandbyTime" "0"
      Option        "SuspendTime" "0"
      Option        "OffTime" "0"
      Option        "BlankTime" "0"
    '';

    xrandrHeads = [
      {
        output = "VGA1";
        monitorConfig = ''
          Option      "Position" "0 0"
          #Option      "PreferredMode" "1920x1080"
        '';
      }
      {
        output = "HDMI1";
        monitorConfig = ''
          Option      "Position" "0 0"
          #Option      "PreferredMode" "1920x1080"
        '';
      }

      {
        output = "DisplayPort-1";
        monitorConfig = ''
          Option      "Position" "0 270"
          Option      "PreferredMode" "1920x1080"
        '';
      }
      {
        output = "DisplayPort-0";
        monitorConfig = ''
          Option      "Position" "0 270"
          Option      "PreferredMode" "1920x1080"
        '';
      }

      {
        output = "HDMI-A-1";
        primary = true;
        monitorConfig = ''
          Option      "Position" "1920 0"
          Option      "PreferredMode" "1920x1080"
          Option      "TransformationMatrix" "1.25 0 0 0 1.25 0 0 0 1"
        '';
      }
      {
        output = "HDMI-A-4";
        monitorConfig = ''
          Option      "Primary" "true"
          Option      "Position" "1920 0"
          Option      "PreferredMode" "1920x1080"
        '';
      }

      {
        output = "HDMI-A-3";
        monitorConfig = ''
          Option      "Position" "4320 270"
          Option      "PreferredMode" "1920x1080"
        '';
      }
      {
        output = "HDMI-A-0";
        monitorConfig = ''
          Option      "Position" "4320 270"
          Option      "PreferredMode" "1920x1080"
        '';
      }
    ];

    videoDrivers = [
      "amdgpu" "intel" #"modesetting"
    ];
  };
}
