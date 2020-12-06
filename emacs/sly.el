{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.sly = {
    enable = false;
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-quicklisp = {
    enable = false;
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-asdf = {
    enable = false;
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-named-readtables = {
    enable = false;
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-macrostep = {
    enable = false;
    demand = true;
    config = ''
    '';
  };
}
