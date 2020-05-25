{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.exwm-input = {
    after = [ "exwm" "map" "cl" ];
    package = epkgs: epkgs.exwm;
    config = ''
      (progn ;;; TODO: Use sd-bus for all of this
        (defstruct service
          "Systemd Service"
          session
          name description
          loadstate activestate substate
          followed path
          jobid jobtype jobpath)

        (defconst bqv/exwm-applications
          (let ((map (make-hash-table)))
            (setf (map-elt map 'firefox)
                  (cons "Firefox" (list "firefox")))
            (setf (map-elt map 'chromium)
                  (cons "Chromium" (list "chromium-safe" "--remote-debugging-port=9222")))
            (setf (map-elt map 'qutebrowser)
                  (cons "QuteBrowser" (list "qutebrowser")))
            (setf (map-elt map 'next)
                  (cons "NextBrowser" (list "bash" "-c" "env $(systemctl --user show-environment | grep DBUS) next")))
            (setf (map-elt map 'gpodder)
                  (cons "GPodder" (list "gpodder")))
            (setf (map-elt map 'pavucontrol)
                  (cons "Pavucontrol" (list "pavucontrol")))
            (setf (map-elt map 'bitwarden)
                  (cons "BitWarden" (list "bitwarden")))
            (setf (map-elt map 'protonmail)
                  (cons "ProtonMail" (list "protonmail-desktop")))
            (setf (map-elt map 'signal)
                  (cons "Signal" (list "signal-desktop")))
            (setf (map-elt map 'riot)
                  (cons "Riot" (list "riot-desktop")))
            (setf (map-elt map 'nheko)
                  (cons "Nheko" (list "nheko")))
            (setf (map-elt map 'slack)
                  (cons "Slack" (list "slack")))
            (setf (map-elt map 'discord)
                  (cons "Discord" (list "Discord")))
            (setf (map-elt map 'ripcord)
                  (cons "Ripcord" (list "ripcord")))
            (setf (map-elt map 'termite)
                  (cons "Termite" (list "termite")))
            map)
          "Maps an application name symbol to a pair (BUFFER-NAME . INVOCATION).")
        ;(bqv/exwm-setup-app)
        (defconst bqv/exwm-services
          (let ((map (make-hash-table)))
            (setf (map-elt map 'protonmail)
                  (cons "protonmail" (list "protonmail-bridge" "--no-window")))
            (setf (map-elt map 'ckb)
                  (cons "ckb-next" (list "ckb-next" "-b")))
            (setf (map-elt map 'clipmenu)
                  (cons "clipmenu" (list "clipmenud")))
            (setf (map-elt map 'dunst)
                  (cons "dunst" (list "dunst")))
            map)
          "Maps an service name symbol to a pair (PROCESS-NAME . INVOCATION).")
        ;(bqv/exwm-setup-svc)

        (defun bqv/systemd-scopes ()
          (mapcar (lambda (scp)
                    (cons (reverse (substring (reverse (car scp)) 6))
                          (make-service :session 'user
                                        :name (nth 1 scp)
                                        :description (nth 2 scp)
                                        :loadstate (nth 3 scp)
                                        :activestate (nth 4 scp)
                                        :substate (nth 5 scp)
                                        :followed (nth 6 scp)
                                        :path (nth 7 scp)
                                        :jobid (nth 8 scp)
                                        :jobtype (nth 9 scp)
                                        :jobpath (nth 10 scp))))
                  (let ((user-mode t))
                    (seq-filter (lambda (srv) (string-suffix-p ".scope" (car srv)))
                                (dbus-call-method
                                 (if user-mode :session :system)
                                 "org.freedesktop.systemd1"
                                 "/org/freedesktop/systemd1"
                                 "org.freedesktop.systemd1.Manager"
                                 "ListUnits"
                                 :timeout 1000)))))

        (defun bqv/systemd-scope-running-p (name)
          (ecase (intern (service-activestate
                          (cdr (assoc name (bqv/systemd-scopes)))))
            (failed nil)
            (running t)))

        (defun bqv/exwm-desktop-invocation (desktop-filename)
          "Launch the application pointed to by DESKTOP-FILENAME."
          (assert (stringp desktop-filename))
          (list "gtk-launch" desktop-filename))

        (defun bqv/exwm-launch (name executable &rest args)
          (let* ((command (cons executable args))
                 (invocation (append `("systemd-run" "--scope" "--user"
                                       ,(concat "--unit=" (downcase name))
                                       "--collect")
                                     command)))
            (with-current-buffer (get-buffer-create (concat "*" name "*"))
              (goto-char (point-max))
              (insert (propertize (format "\n$ %s %s\n"
                                          executable
                                          (string-join args " "))
                                  'face 'bold))
              (apply #'start-process
                     (append `(,name ,(current-buffer))
                             invocation))
              (shell-mode)
              (current-buffer))))

        (defun bqv/exwm-harvest (name)
          (let ((invocation `("systemctl" "--user" "stop"
                              ,(concat (downcase name) ".scope"))))
            (apply #'call-process
                   (append `(,(car invocation) nil nil nil)
                           (cdr invocation)))))

        (defun bqv/exwm-scan (name)
          (let ((invocation `("systemctl" "--user" "show"
                              ,(concat (downcase name) ".scope")
                              "--no-page")))
            (with-temp-buffer
              (apply #'call-process
                     (append `(,(car invocation) nil t nil)
                             (cdr invocation)))
              (goto-char (point-min))
              (not (null (save-excursion (search-forward "Transient=yes" nil t)))))))

        (defun bqv/exwm-screenshot (&rest args)
          (let ((incantation `("systemd-run" "--scope" "--user" "--unit=scrot" "--collect"
                               "scrot" ,@args "-e" "'ipfscat $f; mv $f ~/var/cap/'")))
            (apply #'start-process
                   (append '("scrot" nil) incantation))))

        (defun bqv/exwm-screenshot-all ()
          (interactive)
          (bqv/exwm-screenshot))

        (defun bqv/exwm-screenshot-current ()
          (interactive)
          (bqv/exwm-screenshot-current)
          (bqv/exwm-screenshot "-u"))

        (defun bqv/exwm-screenshot-region ()
          (interactive)
          (bqv/exwm-screenshot "-s"))

        (setenv "PATH" (concat (expand-file-name "~/bin") ":" (getenv "PATH"))) ;; TODO: Remove this hack
        (defun executables-list ()
          "Returns a list of all files available in the directories of the $PATH variable."
          (let ((valid-path (remove-if-not #'file-exists-p (split-string (getenv "PATH") ":"))))
            (remove-if-not #'file-executable-p
                           (remove-if #'file-directory-p
                                      (apply #'append
                                             (mapcar (lambda (p) (directory-files p t)) valid-path))))))

        (defun bqv/exwm-svc-start (name)
          "Start NAME unless already started. NAME is a key of `bqv/exwm-services'."
          (interactive (list (intern (completing-read "Service" (map-keys bqv/exwm-services) nil t))))
          (let* ((service (map-elt bqv/exwm-services name))
                 (buffer-name (concat "*" (car service) "*")))
            (assert (not (null service)))
            (if-let* ((buffer (get-buffer buffer-name)))
              (when (y-or-n-p (format "%s was started already.  Start it again? " (car service)))
                (apply #'bqv/exwm-launch service))
              (apply #'bqv/exwm-launch service))))

        (defun bqv/exwm-app-join (name)
          "Switch to application NAME if already started, start it otherwise.
    NAME is a key of `bqv/exwm-applications'."
          (interactive (list (intern (completing-read "Application" (map-keys bqv/exwm-applications) nil t))))
          (let* ((application (map-elt bqv/exwm-applications name))
                 (buffer-name (car application)))
            (assert (not (null application)))
            (if (and (bqv/exwm-scan (car (cdr application)))
                     (y-or-n-p (format "%s is already started.  Kill it? " buffer-name)))
                (bqv/exwm-harvest (car (cdr application)))
              (apply #'bqv/exwm-launch application))))

        (defun bqv/exwm-exec (cmd)
          "Launch a shell command CMD."
          (interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
          (let* ((executable (car (split-string cmd " ")))
                 (buffer-name (concat "*" executable "*"))
                 (canonical-name (assoc executable (executables-list)
                                        '(lambda (exe) (= (file-name-base exe) )))))
            (assert (not (null executable)))
            (bqv/exwm-launch executable "sh" "-c" cmd)))

        (defun bqv/exwm-sudo-exec (cmd)
          "Launch a shell command CMD through sudo."
          (interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
          (let* ((executable (car (split-string cmd " ")))
                 (buffer-name (concat "*" executable "*"))
                 (canonical-name (assoc executable (executables-list)
                                        '(lambda (exe) (= (file-name-base exe) )))))
            (assert (not (null executable)))
            (bqv/exwm-launch executable "sudo" "sh" "-c" cmd)))

        (defun bqv/exwm-nix-exec (cmd)
          "Launch a shell command CMD through nix-shell."
          (interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
          (let* ((executable (car (split-string cmd " ")))
                 (buffer-name (concat "*" executable "*"))
                 (canonical-name (assoc executable (executables-list)
                                        '(lambda (exe) (= (file-name-base exe) )))))
            (assert (not (null executable)))
            (bqv/exwm-launch executable "nix-shell" "-p" executable "--run" cmd)))

        (defun bqv/exwm-setup-app ()
          "Setup app namespace"
          (with-demoted-errors "Exwm: %S"
            (dolist (app (ht-keys bqv/exwm-applications))
              (eval `(defun ,(intern (format "app/%s" app)) ()
                       (interactive)
                       (bqv/exwm-app-join ',app))))))

        (defun bqv/exwm-setup-games ()
          "Setup game namespace"
          (with-demoted-errors "Exwm: %S"
            (setq gnutls-log-level 1)
            (unless (and (boundp 'steam-games) steam-games) (steam-get-games))
            (when (boundp 'steam-games)
              (dolist (game (mapcar
                             (lambda (game)
                               (cons (steam-game-attribute game 'name)
                                     (steam-game-attribute game 'appID)))
                             steam-games))
                (when game
                  (eval `(defun ,(intern (format "game/%s"
                                                 (replace-regexp-in-string (regexp-quote " ") "-"
                                                                           (downcase (car game))
                                                                           nil 'literal))) ()
                           (interactive)
                           (steam-launch-id ,(cdr game)))))))))

        (defun bqv/exwm-setup-desktop ()
          "Setup exwm desktop"
          (with-demoted-errors "Exwm: %S"
            (bqv/exwm-svc-start 'ckb)
            (bqv/exwm-svc-start 'clipmenu)
            (bqv/exwm-svc-start 'dunst)
            (bqv/exwm-svc-start 'protonmail))
          (with-demoted-errors "Exwm: %S"
            (bqv/exwm-app-join 'firefox)
            (bqv/exwm-app-join 'riot)))

        (add-hook 'exwm-init-hook 'bqv/exwm-setup-app)
       ;(add-hook 'exwm-init-hook 'bqv/exwm-setup-games)
        (add-hook 'exwm-init-hook 'bqv/exwm-setup-desktop)
        (bind-key  "C-. s f" (lambda () (interactive) (bqv/exwm-app-join 'firefox)))
        (bind-key  "C-. s c" (lambda () (interactive) (bqv/exwm-app-join 'chromium)))
        (bind-key  "C-. s k" (lambda () (interactive) (bqv/exwm-app-join 'slack))))

      (progn
        ;; Key bindings accessible from everywhere:
        (exwm-input-set-key (kbd "s-r") #'exwm-reset)
        (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
        (exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)
        (exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
        (exwm-input-set-key (kbd "s-;") #'other-frame)

        (exwm-input-set-key (kbd "<s-tab>") #'bqv/swap-last-buffers)
        (exwm-input-set-key (kbd "C-x w") #'bqv/switch-to-window)
        (exwm-input-set-key (kbd "C-;") #'other-window)
        (exwm-input-set-key (kbd "s-!") #'counsel-linux-app)
        (exwm-input-set-key (kbd "s-&") #'bqv/exwm-exec)
        (exwm-input-set-key (kbd "C-M-'") #'shell-switcher-new-shell)
        (exwm-input-set-key (kbd "C-'") #'shell-switcher-switch-buffer)
        (exwm-input-set-key (kbd "C-M-v") #'scroll-other-window)

        ;; Bind C-q so that the next key is sent literally to the
        ;; application
        (add-to-list 'exwm-input-prefix-keys ?\C-q)
        (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

        (add-to-list 'exwm-input-prefix-keys ?\C-.)
        (add-to-list 'exwm-input-prefix-keys ?\C-,)

        (setq exwm-input-simulation-keys
              `(
                ;; movement
                ([?\C-b] . [left])
                ([?\M-b] . [C-left])
                ([?\C-f] . [right])
                ([?\M-f] . [C-right])
                ([?\C-p] . [up])
                ([?\C-n] . [down])
                ([?\C-a] . [home])
                ([S-left] . [C-home])
                ([S-right] . [C-end])
                ([?\C-e] . [end])
                ([?\M-v] . [prior])
                ([?\C-v] . [next])
                ([?\C-d] . [delete])
                ([?\C-k] . [S-end ?\C-x])
                ;; cut/paste, selection
                ([?\C-w] . [?\C-x])
                ([?\M-w] . [?\C-c])
                ([?\C-y] . [?\C-v])
                ([?\M-d] . [C-S-right ?\C-x])
                ([M-backspace] . [C-S-left ?\C-x])
                ;; search
                ([?\C-s] . [?\C-f])
                ;; escape
                ([?\C-g] . [escape]))))
    '';
  };
}
