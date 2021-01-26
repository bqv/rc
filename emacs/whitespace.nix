{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.whitespace = {
    demand = true;
    package = lib.const null;
    config = ''
      (add-hook 'prog-mode-hook 'whitespace-mode)
      (delq 'lines whitespace-style)
      (add-to-list 'whitespace-style 'lines-tail)
    '';
  };
}
