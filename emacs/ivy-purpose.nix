{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.ivy-purpose = {
    demand = true;
    after = [ "ivy" "window-purpose" "bufler" ];
    config = ''
      (ivy-purpose-setup)
      ;(define-key purpose-mode-map "C-x b" nil)
      (define-purpose-prefix-overload purpose-switch-buffer-overload
        '(bufler-switch-buffer ;ivy-purpose-switch-buffer-without-purpose
          ivy-switch-buffer
          ivy-purpose-switch-buffer-with-some-purpose))
    '';
  };
}
