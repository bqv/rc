{ config, pkgs, ... }:

{
  services.guix.enable = true;
  services.guix.package = pkgs.guix;
}
