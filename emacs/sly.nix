{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.sly = {
    demand = true;
    require = [];
    config = ''
      nil
    '';
  };
  emacs.loader.sly-quicklisp = {
    demand = true;
    after [ "sly" ];
    config = ''
      nil
    '';
  };
  emacs.loader.sly-asdf = {
    demand = true;
    after [ "sly" ];
    config = ''
      nil
    '';
  };
  emacs.loader.sly-named-readtables = {
    demand = true;
    after [ "sly" ];
    config = ''
      nil
    '';
  };
  emacs.loader.sly-macrostep = {
    demand = true;
    after [ "sly" ];
    config = ''
      nil
    '';
  };
}
