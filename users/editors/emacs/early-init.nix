{ config, lib, usr, ... }:

usr.elisp.writeFile {
  name = "early-init";
  description = "Executed before initialization.";
  text = let
    secrets = import ../../../secrets/emacs.user.nix;
  in ''
    (setq debug-on-error t)

    (progn ; user
      (setq user-full-name "${secrets.user-full-name}")
      (setq user-mail-address "${secrets.user-mail-address}"))

    (progn ; performance
      (setq gc-cons-threshold 50000000)
      (setq large-file-warning-threshold 100000000))

    (progn ; debug
      (define-key special-event-map [sigusr1] 'keyboard-quit)
      (define-key special-event-map [sigusr2] 'keyboard-escape-quit))
  '';
}
