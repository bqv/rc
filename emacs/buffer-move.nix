{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.buffer-move = {
    demand = true;
    after = [ "exwm-input" ];
    config = ''
      (progn
        (exwm-input-set-key (kbd "<s-up>") #'buf-move-up)
        (exwm-input-set-key (kbd "<s-down>") #'buf-move-down)
        (exwm-input-set-key (kbd "<s-left>") #'buf-move-left)
        (exwm-input-set-key (kbd "<s-right>") #'buf-move-right))
    '';
  };
}
