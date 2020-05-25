{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.exwm-randr = {
    after = [ "exwm" "exwm-workspace" "exwm-manage" ];
    package = epkgs: epkgs.exwm;
    config = ''
      (progn
        (defun bqv/exwm-xrandr ()
          "Configure screen with xrandr."
          (setq xrandr-current
                (shell-command-to-string "xrandr")))
        (defun bqv/exwm-pinning ()
          (progn
            (ignore-errors
              (let* ((wmap exwm-workspace-window-assignments)
                     (target (cdr (assoc exwm-class-name wmap))))
                (exwm-workspace-move-window target)))
            (ignore-errors
              (if (string= (substring exwm-class-name 0 10) "steam_app_")
                (exwm-workspace-move-window 1))))))
      (progn
        (defvar exwm-workspace-window-assignments
          '(("chromium-browser-chromium" . 2)
            ("riot" . 0)
            ("ansi-term" . 0))
          "An alist of windows and what workspace to put them on.")
        (setq exwm-randr-workspace-monitor-plist
              (let ((screens (mapcar 'car bqv/exwm-screens))
                    (monitor-plist nil)
                    (index 0))
                (while screens
                  (setq monitor-plist (append monitor-plist `(,index ,(car screens))))
                  (setq index (+ 1 index))
                  (setq screens (cdr screens)))
                monitor-plist)))
              ;'(0 "DisplayPort-1" 1 "DVI-D-0" 2 "HDMI-A-3" 3 "HDMI-A-4")))
              ;; '(0 "DisplayPort-1" 1 "HDMI-A-3" 2 "HDMI-A-4")))
      (progn
        (add-hook 'exwm-randr-screen-change-hook 'bqv/exwm-xrandr)
        (add-hook 'exwm-manage-finish-hook 'bqv/exwm-pinning)
        (exwm-randr-enable))
    '';
  };
}
