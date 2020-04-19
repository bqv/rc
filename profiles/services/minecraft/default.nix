{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  services.minecraft-server.enable = true;
  services.minecraft-server.openFirewall = true;
  services.minecraft-server.eula = true;
  services.minecraft-server.package = with pkgs;
    minecraft-server.overrideAttrs (super: rec {
      version = "1.7.5";
      src = fetchurl {
        url    = "http://s3.amazonaws.com/Minecraft.Download/versions/${version}/minecraft_server.${version}.jar";
        sha256 = "1r22fn9k4650lzx87nsc4kz04mf00762xh05acxjn49jxqxf3afa";
      };
    });
}
