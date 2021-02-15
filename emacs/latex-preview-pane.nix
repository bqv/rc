{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.latex-preview-pane = {
    demand = true;
    after = [ "tex" "latex" ];
  };
}
