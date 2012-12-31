{ config, lib, pkgs, usr, ... }:

{
  services.hydroxide = {
    enable = true;
    userauths = lib.mapAttrs (_: u: u.auth) (usr.secrets.hydroxide.auth);
  };
}
