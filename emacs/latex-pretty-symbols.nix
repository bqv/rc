{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.latex-pretty-symbols = {
    demand = true;
    after = [ "tex" "latex" ];
  };
}
