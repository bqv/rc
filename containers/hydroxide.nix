{ config, pkgs, lib, ... }:

let
  hostAddress = "10.8.0.1";
  localAddress = "10.8.0.2";
in {
  containers.hydroxide =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { config, stdenv, ... }:
 
        {
          imports = [ 
            ../modules/services/hydroxide
            ../profiles/services/hydroxide
          ];

          nixpkgs.pkgs = pkgs;

          environment.systemPackages = with pkgs; [
            hydroxide
          ];

          services.hydroxide.host = localAddress;
        };
    };
}
