{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.git-gutter = {
    demand = true;
    diminish = [ "git-gutter-mode" ];
    config = ''
      (global-git-gutter-mode 't)
    '';
  };
}
