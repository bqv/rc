{ config, pkgs, lib, ... }:

let
  hostAddress = "10.9.0.1";
  localAddress = "10.9.0.2";
in {
  containers.anki =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { config, stdenv, ... }:
 
        {
          nixpkgs.pkgs = pkgs;

          environment.systemPackages = with pkgs; [
            ankisyncd
          ];

          services.ankisyncd = {
            enable = true;
            host = localAddress;
            openFirewall = false;
            port = 27701;
          };
        };
    };
}
