{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.latex-extra = {
    demand = true;
    after = [ "tex" "latex" ];
  };
}
