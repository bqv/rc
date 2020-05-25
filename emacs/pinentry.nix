{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.pinentry = {
    demand = true;
    after = [ "exwm" ];
    config = ''
      (progn
        ;; Use gpg as ssh agent (for this to work in shells, also set the
        ;; ENV var in $HOME/.profile or similar).
        (setenv "SSH_AUTH_SOCK"
                (string-trim
                 (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))))
      (progn
        (pinentry-start)

        (defun bqv/pinentry-restart ()
          "Kill and restart gpg-agent and pinentry."
          (interactive)
          (delete-process "pinentry")
          (shell-command "gpgconf --kill gpg-agent")
          (pinentry-start)
          (message "gpg-agent and pinentry restarted successfully.")))
    '';
  };
}
