{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.switch-window = {
    demand = true;
    after = [ "exwm" "exwm-input" ];
    bind = {
      "s-m" = "switch-window";
    };
    config = ''
      (exwm-input-set-key (kbd "s-m") 'switch-window)
      (exwm-input--update-global-prefix-keys)
      (setq switch-window-shortcut-style 'qwerty
            switch-window-input-style 'minibuffer
            switch-window-multiple-frames t)
    '';
  };
}
