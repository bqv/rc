{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.omnisharp = {
    demand = true;
    systemDeps = with pkgs; [ omnisharp-roslyn ];
  };
}
