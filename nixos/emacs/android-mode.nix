{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.android-mode = {
    demand = true;
    config = ''
      (setq-default adb-host "192.168.0.3")
      (defun adb-connect (host)
        "Connect ADB Daemon to HOST."
        (interactive (list (read-string (format "Android Device Host [%s]: " adb-host) nil nil adb-host)))
        (let ((adb (android-tool-path "adb")))
          (with-temp-buffer
            (let ((exit-code (call-process adb nil (current-buffer) nil "connect" host)))
              (progn
                (goto-char (point-max))
                (delete-backward-char 1)
                (message "%s: %s" adb (buffer-string)))
              (when (= exit-code 0)
                (setq adb-host host))))))
    '';
  };
}
