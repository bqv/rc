{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.exwm-manage = {
    demand = true;
    after = [ "exwm" ];
    package = epkgs: epkgs.exwm;
    config = ''
      (progn
        ;; https://github.com/ch11ng/exwm/issues/574
        (add-to-list 'exwm-manage-configurations
                     '((equal exwm-class-name "Slack")
                       managed t))
        (add-to-list 'exwm-manage-configurations
                     '((equal exwm-class-name "Riot")
                       workspace 0))
        (add-to-list 'exwm-manage-configurations
                     '((or (equal exwm-instance-name "qutebrowser")
                           (equal exwm-class-name "Firefox"))
                       workspace 2)))
    '';
  };
}
