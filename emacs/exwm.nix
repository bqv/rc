{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.exwm = {
    demand = true;
    config = ''
      (progn
        (defun bqv/exwm-reliable-class-p ()
          "Return t if application's class is suitable for naming."
          (and (not (string-prefix-p "sun-awt-X11-" exwm-instance-name))
               ;; gimp has several windows with the same class:
               (not (string= "gimp" exwm-instance-name))))

        (defun bqv/exwm-class-updated ()
          "Use class names if `bqv/exwm-reliable-class-p'."
          (when (bqv/exwm-reliable-class-p)
            (exwm-workspace-rename-buffer
             (case exwm-class-name
               (("chromium-browser-chromium") "chromium")
               ("Firefox" (if (string= "Picture-in-Picture" exwm-title)
                                "Video"
                              "Firefox"))
               (otherwise exwm-class-name)))))

        (defun bqv/exwm-title-updated ()
          "Use title unless `bqv/exwm-reliable-class-p'."
          (unless (bqv/exwm-reliable-class-p)
            (exwm-workspace-rename-buffer
             (case exwm-class-name
               (("chromium-browser-chromium") "chromium")
               (otherwise exwm-class-name)))))

       ;(setq bqv/exwm-screens
       ;      '(("DisplayPort-1" ("1920x1080" "0x270" nil))
       ;        ("HDMI-A-4" ("1920x1080" "1920x0" "1.25x1.25"))
       ;        ("HDMI-A-3" ("1920x1080" "4320x270" nil))))
        (setq bqv/exwm-screens
              '(("DisplayPort-0" ("1920x1080" "0x270" nil))
                ("HDMI-A-1" ("1920x1080" "1920x0" "1.25x1.25"))
                ("HDMI-A-0" ("1920x1080" "4320x270" nil))))

        (defun bqv/exwm-setup ()
          "Setup exwm instance"
          (with-demoted-errors "Exwm: %S"
            (setq display-time-day-and-date t)
            (display-time-mode t))
          (with-demoted-errors "Exwm: %S"
            (with-temp-buffer
              (shell-command
                (format "xrandr %s"
                        (mapconcat (lambda (scr)
                                     (let ((output (car scr))
                                           (mode (car (cadr scr)))
                                           (pos (cadr (cadr scr)))
                                           (style (caddr (cadr scr))))
                                       (format "%s %s %s %s"
                                               (format "--output %s" output)
                                               (format "--mode %s" mode)
                                               (format "--pos %s" pos)
                                               (if (null style)
                                                   "--auto"
                                                 (format "--scale %s" style)))))
                                   bqv/exwm-screens
                                   " ")))))
          (with-demoted-errors "Exwm: %S"
            (desktop-environment-volume-set "30%"))
          nil)

        (defun bqv/exwm-init-advice (old-function &rest args)
          (cl-flet ((rawdisplay
                     (str)
                     (let ((dsp (replace-regexp-in-string "^:\\|\\.0$" "" str)))
                       (string-to-number dsp))))
               (unless (>= 50 (rawdisplay
                               (frame-parameter (selected-frame) 'display)))
                   (apply old-function args)))))
      ;(advice-add 'exwm-init
      ;            :around 'bqv/exwm-init-advice)
      (add-hook 'exwm-init-hook 'bqv/exwm-setup)
      (add-hook 'exwm-update-class-hook 'bqv/exwm-class-updated)
      (add-hook 'exwm-update-title-hook 'bqv/exwm-title-updated)
      (setq exwm-debug-on t)
      (exwm-enable)
    '';
  };
}
