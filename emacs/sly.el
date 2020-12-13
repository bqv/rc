{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.sly = {
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-quicklisp = {
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-asdf = {
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-named-readtables = {
    demand = true;
    config = ''
    '';
  };
  emacs-loader.sly-macrostep = {
    demand = true;
    config = ''
    '';
  };
}
