{ config, lib, pkgs, domains, ... }:

with lib; {
  config = {
    home.packages = with pkgs; [ darcs ];
  };
}
