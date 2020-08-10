{ config, pkgs, ... }:

{
  services.aria2.enable = true;
  services.aria2.downloadDir = "/srv";
}
