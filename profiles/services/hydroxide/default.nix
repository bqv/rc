{ config, lib, pkgs, domains, ... }:

{
  services.hydroxide = {
    enable = true;
    userauths = lib.mapAttrs (_: u: u.auth) (import ../../../secrets/hydroxide.auth.nix);
  };
}
