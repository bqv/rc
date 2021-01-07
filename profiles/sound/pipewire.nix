{ config, lib, pkgs, ... }:

{
  hardware.pulseaudio.enable = lib.mkForce false;
  services.jack.jackd.enable = lib.mkForce false;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };

  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };

  environment.systemPackages = with pkgs; [
    # ALSA Tools
    # ------
    alsaUtils

    # PulseAudio control
    # ------------------
    ncpamixer
    pavucontrol
    pulseeffects
    lxqt.pavucontrol-qt
    pasystray
  ];
}
