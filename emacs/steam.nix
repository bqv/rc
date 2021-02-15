{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.steam = {
    demand = true;
    config = let
      creds = usr.secrets.steam.credentials;
    in ''
      (setq steam-username "${creds.user}")
    '';
  };
}
