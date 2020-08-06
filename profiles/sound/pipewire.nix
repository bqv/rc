{ config, lib, pkgs, ... }:

{
  services.pipewire = {
    enable = true;
  };
}
