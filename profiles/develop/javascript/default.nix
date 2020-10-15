{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nodejs
    nodePackages.http-server
  ];
}
