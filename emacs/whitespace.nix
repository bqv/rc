{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.whitespace = {
    demand = true;
    package = lib.const null;
    config = ''
      (add-hook 'prog-mode-hook 'whitespace-mode)
      (delq whitespace-style 'lines)
      (add-to-list 'whitespace-style 'lines-tail)
    '';
  };
}
