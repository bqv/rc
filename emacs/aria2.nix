{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.aria2 = {
    demand = true;
    config = ''
      (setq aria2-rcp-secret "aria2rpc")
    '';
  };
}
