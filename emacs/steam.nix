{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.steam = {
    demand = true;
    config = let
      creds = import ../secrets/steam.credentials.nix;
    in ''
      (setq steam-username "${creds.user}")
    '';
  };
}
