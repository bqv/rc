{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.erc = {
    demand = true;
    package = lib.const null;
    config = ''
      (require 'erc-networks)

      ;(setq erc-hide-list '("JOIN" "PART" "QUIT"))
      (setq erc-rename-buffers t)
      (setq erc-prompt (lambda () (concat "[" (symbol-name (erc-network)) "/" (buffer-name) "]")))
      (setq erc-track-exclude-types
            '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353"
              "324" "329" "332" "447"))

      (add-hook 'erc-mode-hook #'outline-minor-mode)
      (add-hook 'erc-mode-hook
                (lambda (_) (setq-local outline-regexp "^<.*>\\|^\\[.*\\] "))
                )

      (defmacro unpack-color (color red green blue &rest body)
        `(let ((,red   (car ,color))
               (,green (car (cdr ,color)))
               (,blue  (car (cdr (cdr ,color)))))
           ,@body))

      (defun rgb-to-html (color)
        (unpack-color color red green blue
                      (concat "#" (format "%02x%02x%02x" red green blue))))

      (defun hexcolor-luminance (color)
        (unpack-color color red green blue
                      (floor (+ (* 0.299 red) (* 0.587 green) (* 0.114 blue)))))

      (defun invert-color (color)
        (unpack-color color red green blue
                      `(,(- 255 red) ,(- 255 green) ,(- 255 blue))))

      (defun erc-get-color-for-nick (nick dark)
        (let* ((hash     (md5 (downcase nick)))
               (red      (mod (string-to-number (substring hash 0 10) 16) 256))
               (blue     (mod (string-to-number (substring hash 10 20) 16) 256))
               (green    (mod (string-to-number (substring hash 20 30) 16) 256))
               (color    `(,red ,green ,blue)))
          (rgb-to-html (if (if dark (< (hexcolor-luminance color) 85)
                             (> (hexcolor-luminance color) 170))
                           (invert-color color)
                         color))))

      (defun erc-highlight-nicknames ()
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\w+" nil t)
            (let* ((bounds (bounds-of-thing-at-point 'symbol))
                   (nick   (buffer-substring-no-properties (car bounds) (cdr bounds))))
              (when (erc-get-server-user nick)
                (put-text-property
                 (car bounds) (cdr bounds) 'face
                 (cons 'foreground-color (erc-get-color-for-nick nick 't))))))))

      (add-hook 'erc-insert-modify-hook 'erc-highlight-nicknames)

      ;; adapt https://www.emacswiki.org/emacs/rcirc-random-names.el ?

      (defun erc-weechat-connect (server network)
        "Connect to SERVER as a weechat ssl irc proxy.
      Gets password from `auth-sources' stored under host ssl.irc.weechat;
      and my-weechat-server.example.com:9001 hardcoded here.
      Will not connect if we already have a connection to NETWORK."
        (if-let ((existing (erc-buffer-list (lambda () (eq (erc-network) network)))))
            (message "Already connected to %s (%S), see buffer %S" network server (car existing))
          (let ((password (auth-source-pick-first-password :user '("weechat")
                                                           :type 'netrc
                                                           :max 1)))
            (erc-tls :server "localhost"
                     :port 6699
                     :password (concat server ":" password)))))

      (defmacro erc-weechat-make-connect (server network)
        "Partially apply `erc-weechat-connect' on a certain SERVER and NETWORK."
        `(defun ,(intern (concat "erc-weechat-connect-" server)) ()
           ,(concat "Connect to " server " on weechat, through ERC.
Will not connect if we already have a connection to NETWORK.")
           (interactive)
           (erc-weechat-connect ,server ,network)))

      (erc-weechat-make-connect "freenode" 'freenode)
    '';
  };
}