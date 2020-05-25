{ config, lib, usr, pkgs, domains, ... }:

{
  emacs-loader.bitwarden = {
    config = ''
      (setenv "BW_SESSION"
              (let ((secret (plist-get
                             (nth 0 (auth-source-search :host "bitwarden-session"))
                             :secret)))
                (if (functionp secret) (funcall secret) secret)))
      (setq bitwarden-user "me@${domains.home}")
      (setq bitwarden-automatic-unlock (lambda () (read-passwd "Bitwarden Password: ")))
    '';
  };
}
