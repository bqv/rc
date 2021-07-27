{ config, pkgs, ... }:

{
  hardware.bluetooth.enable = true;

  # Enable bluetooth daemon.
  services.blueman.enable = true;
}
