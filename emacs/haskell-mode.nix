{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.haskell-mode = {
    demand = true;
    require = [ "haskell" "haskell-doc" ];
    after = [ ];
    config = ''
      (add-to-list 'auto-mode-alist '("\\.tpl" . mhtml-mode))
    '';
  };
}
