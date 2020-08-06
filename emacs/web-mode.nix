{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.web-mode = {
    demand = true;
    mode = {
      "\"\\\\.cshtml\\\\'\"" = "web-mode";
    };
  };
}
