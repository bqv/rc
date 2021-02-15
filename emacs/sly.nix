{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.sly = {
    demand = true;
    config = ''
      nil
    '';
  };
  emacs.loader.sly-quicklisp = {
    demand = true;
    config = ''
      nil
    '';
  };
  emacs.loader.sly-asdf = {
    demand = true;
    config = ''
      nil
    '';
  };
  emacs.loader.sly-named-readtables = {
    demand = true;
    config = ''
      nil
    '';
  };
  emacs.loader.sly-macrostep = {
    demand = true;
    config = ''
      nil
    '';
  };
}
