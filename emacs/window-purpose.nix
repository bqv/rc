{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.window-purpose = {
    demand = true;
    config = ''
      (add-to-list 'purpose-user-mode-purposes '(weechat-mode . chat))
      (add-to-list 'purpose-user-name-purposes '("qutebrowser" . web))
      (add-to-list 'purpose-user-regexp-purposes '("^termite.*" . terminal))
      (setq purpose-use-default-configuration t)
      (purpose-compile-user-configuration)
      (purpose-mode)
    '';
  };
}
