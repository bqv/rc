{ super, config, lib, pkgs, ... }:

{
  config = {
    home.file.".config/nyxt/init.lisp".force = true;
    home.file.".config/nyxt/init.lisp".text = let
      secrets = import ../../../secrets/nyxt.autofill.nix;
    in ''
      (unless (and (boundp '*swank-started*) *swank-started*)
        (setq *swank-port* 4005)
        (ignore-errors
         (start-swank)
         (uiop:launch-program (list "emacsclient" "--eval" "(nyxt-repl)"))
         (list "emacsclient" "--eval" "(setq slime-enable-evaluate-in-emacs t)")
         (setq *swank-started* t)))

      (define-mode shell-mode ()
          "A basic shell prompt."
          ((keymap-schemes
            :initform
            (let ((map (make-keymap "shell-mode-map")))
              (define-key map
                "c" 'run-shell-command
                "k" 'clear-shell)
              (list :emacs map
                    :vi-normal map)))))

      (define-parenscript clear-shell-output ()
          (setf (ps:chain document body inner-h-t-m-l) ""))

      ;(define-command clear-shell (shell-mode)
      ;  "Clear the output in the shell buffer."
      ;  (ipc-buffer-evaluate-javascript
      ;   (active-buffer *browser*)
      ;   (clear-shell-output)))

      (define-parenscript append-output (output)
        (setf (ps:chain document body inner-h-t-m-l)
              (ps:chain document body inner-h-t-m-l
                        (concat (ps:lisp
                                 (format nil "<pre><code>~a</code></pre><br/>" output))))))

      ;(define-command run-shell-command (shell-mode)
      ;  "Run a shell command."
      ;  (with-result
      ;      (input (read-from-minibuffer
      ;              (minibuffer *browser*)
      ;              :input-prompt "Run in shell:"))
      ;    (ipc-buffer-evaluate-javascript
      ;     (active-buffer *browser*)
      ;     (append-output
      ;      :output
      ;      (uiop:run-program input :force-shell t :output :string)))))

      ;(define-command shell ()
      ;  "Open a shell buffer."
      ;  (set-active-buffer *browser* (make-buffer :name "*shell*" :default-modes '(shell-mode))))

      (define-parenscript %reload-page ()
        (ps:chain location (reload)))

      (define-command reload-page ()
        "Reload page."
        (%reload-page))

      ; Scroll parenscripts
      (define-parenscript %scroll-page-down ()
        (ps:chain window (scroll-by 0 (ps:@ window inner-height))))

      (define-parenscript %scroll-page-up ()
        (ps:chain window (scroll-by 0 (- (ps:@ window inner-height)))))

      (define-command scroll-page-down ()
        "Scroll down by one page height."
        (%scroll-page-down))

      (define-command scroll-page-up ()
        "Scroll up by one page height."
        (%scroll-page-up))

      (defun emacs-paste ()
        (swank::eval-in-emacs '(substring-no-properties (current-kill 0))))
      (defun emacs-copy (s)
        (swank::eval-in-emacs `(kill-new ,s)))
      (defun message (s)
        (swank::eval-in-emacs `(message ,s)))

      (define-command paste-from-emacs ()
        "Paste text from emacs kill-ring."
        (swank::eval-in-emacs `(nyxt-push (substring-no-properties (current-kill 0))))
        (%paste :input-text (containers:first-item (clipboard-ring *browser*))))

      (define-command copy-to-emacs ()
        "Copy text to emacs kill-ring."
        (with-result (input (%copy))
          (containers:insert-item (clipboard-ring *browser*) input))
        (swank::eval-in-emacs `(nyxt-pull)))

      (define-command copy-url-to-emacs ()
        "Copy url to emacs kill-ring."
        (with-result (uri (object-string (url (current-buffer))))
          (containers:insert-item (clipboard-ring *browser*) uri))
        (swank::eval-in-emacs `(nyxt-pull)))

      (define-parenscript %bw-autofill (username password)
        (defun is-visible (elem)
          (ps:let ((style (ps:chain elem owner-document default-view (get-computed-style elem null))))
            (ps:if (ps:or (ps:not (ps:eql (ps:chain style (get-property-value "visibility")) "visible"))
                          (ps:eql (ps:chain style (get-property-value "display")) "none")
                          (ps:eql (ps:chain style (get-property-value "opacity")) "0"))
                   ps:f
                   (ps:and (> (ps:@ elem offset-width) 0) (> (ps:@ elem offset-height) 0)))))

        (defun has-password-field (form)
          (ps:let ((inputs (ps:chain form (get-elements-by-tag-name "input"))))
            (ps:dolist (input inputs)
              (ps:when (ps:eql (ps:@ input type) "password")
                (ps:return ps:t)))))

        (defun update-form-data (form username password)
          (ps:let ((inputs (ps:chain form (get-elements-by-tag-name "input"))))
            (ps:dolist (input inputs)
              (ps:when (ps:and (is-visible input)
                               (ps:or (ps:= (ps:@ input 'type) "text")
                                      (ps:= (ps:@ input 'type) "email")))
                (ps:chain input (focus))
                (setf (ps:@ input 'value) username)
                (ps:chain input (blur)))
              (ps:when (ps:= (ps:@ input 'type) "password")
                (ps:chain input (focus))
                (setf (ps:@ input 'value) password)
                (ps:chain input (blur))))))

        (ps:let ((forms (ps:chain document (get-elements-by-tag-name "form"))))
          (ps:dolist (form forms)
            (ps:if (has-password-field form)
                   (update-form-data form (ps:lisp username) (ps:lisp password))))))

      (defvar *bw-session*
        (ignore-errors
         (string-right-trim '(#\newline) (uiop:read-file-string "~/.bwrc")))
        "Bitwarden session key")

      (defun bw-unlock ()
        (case (bw-status *bw-session*)
          (unlocked *bw-session*)
          (locked (setq *bw-session*
                        (with-result (password (read-from-minibuffer
                                                (make-minibuffer :input-prompt "[Bitwarden] Master Password:")))
                          (uiop:run-program
                           (list "${pkgs.bitwarden-cli}/bin/bw" "unlock" password "--raw")
                           :output :string))))))

      (defun bw-status (key)
        (let* ((output (uiop:run-program
                        (list "${pkgs.bitwarden-cli}/bin/bw" "--session" key "status")
                        :output :string
                        :ignore-error-status t))
               (status (cdr (assoc :status (ignore-errors
                                            (cl-json:decode-json-from-string
                                             output))))))
          (unless (null status)
            (intern (string-upcase status)))))

      (defun bw-search (key term)
        (cl-json:decode-json-from-string
         (uiop:run-program
          (list "${pkgs.bitwarden-cli}/bin/bw" "--session" key "list" "items" "--search" term)
          :output :string
          :ignore-error-status t)))

      (defun bw-item-filter (key term)
        (lambda (minibuffer)
          (let* ((search-json (bw-search key term))
                 (item-displays (mapcar
                                 (lambda (entry)
                                   (let* ((name (cdr (assoc :name entry)))
                                          (login (cdr (assoc :login entry)))
                                          (user (cdr (assoc :username login))))
                                     (format nil "~a:~a" user name)))
                                 search-json)))
            (fuzzy-match (input-buffer minibuffer) search-json :suggestions-display item-displays))))

      (defun bw-copy-totp (totp)
        nil)

      (define-command bitwarden-select-password ()
        (let ((term (quri:uri-host (url (current-buffer)))))
          (with-result (password-item (read-from-minibuffer
                                       (make-minibuffer :input-prompt "Bitwarden"
                                                        :suggestion-function
                                                        (bw-item-filter (bw-unlock) term))))
            (let* ((login (cdr (assoc :login password-item)))
                   (user (cdr (assoc :username login)))
                   (pass (cdr (assoc :password login)))
                   (totp (cdr (assoc :totp login))))
              (%bw-autofill user pass)
              (bw-copy-totp totp)))))

      (let ((handlers (list ;; doi://path -> https url
                       (url-dispatching-handler
                        'doi-link-dispatcher (match-scheme "doi")
                        (lambda (url)
                          (quri:uri (format nil "https://doi.org/~a"
                                            (quri:uri-path url)))))

                       ;; github://owner/repo -> https url
                       (url-dispatching-handler
                        'github-link-dispatcher (match-scheme "github" "gh")
                        (lambda (url)
                          (quri:uri (format nil "https://github.com/~a"
                                            (quri:uri-path url)))))

                       ;; magnet:uri -> aria2
                       (url-dispatching-handler
                        'aria2-magnet-links (match-scheme "magnet")
                        (lambda (url)
                          (uiop:launch-program
                           (list "${pkgs.python3Packages.aria2p}/bin/aria2p"
                                 "--secret" "${super.services.aria2.rpcSecret}"
                                 "add-magnets" (object-string url)))
                          nil))

                       ;; youtube -> mpv
                       (url-dispatching-handler
                        'mpv-youtube-links (match-domain "youtube.com" "m.youtube.com" "youtu.be")
                        (lambda (url)
                          (uiop:launch-program
                           (list "${pkgs.mpv}/bin/mpv"
                                 (object-string url)))
                          nil))

                       ;; twitch -> mpv
                       (url-dispatching-handler
                        'mpv-twitch-links (match-domain "twitch.tv")
                        (lambda (url)
                          (uiop:launch-program
                           (list "${pkgs.mpv}/bin/mpv"
                                 "--no-cache" "--cache-secs=0" "--demuxer-readahead-secs=0" "--untimed"
                                 "--cache-pause=no"
                                 (object-string url)))
                          nil))
                       )))
        (define-mode dispatch-mode ()
          "Mode to intercept and redirect URIs externally."
          ((destructor
            :initform
            (lambda (mode)
              (reduce (lambda (hook el) (hooks:remove-hook hook el) (request-resource-hook (buffer mode)))
                      handlers :initial-value (request-resource-hook (buffer mode)))))
           (constructor
            :initform
            (lambda (mode)
              (unless (request-resource-hook (buffer mode))
                (make-hook-resource
                 :combination #'combine-composed-hook-until-nil
                 :handlers '()))
              (reduce (lambda (hook el) (hooks:add-hook hook el))
                      handlers :initial-value (request-resource-hook (buffer mode))))))))

      (unless (and (boundp '*configured-buffer*) *configured-buffer*)
        (define-configuration buffer
          ((keymap-scheme-name scheme:emacs)
           (default-new-buffer-url "about:blank")
           (override-map
            (let ((map (make-keymap "override-map")))
             ;(define-key map "C-x s" 'shell)
              (define-key map
                "C-v" 'scroll-page-down
                "M-v" 'scroll-page-up)
              (define-key map
                "C-k" 'copy-to-emacs
                "C-y" 'paste-from-emacs)
              (define-key map "M-h" 'nyxt/web-mode:buffer-history-tree)
             ;(define-key map "button4" 'history-backwards)
             ;(define-key map "button5" 'history-forwards)
             ;(define-key map "button6" 'delete-current-buffer)
             ;(define-key map "button7" 'switch-buffor-previous)
             ;(define-key map "button8" 'switch-buffor-next)
              map))
           (default-modes (append '(dispatch-mode blocker-mode) %slot-default))
           ))
        (setq *configured-buffer* t))

      (unless (and (boundp '*configured-browser*) *configured-browser*)
        (define-configuration browser
          ((session-restore-prompt :always-restore)
           (external-editor-program "gnvim")
           (download-path (make-instance 'download-data-path :dirname "~/tmp/"))
           (search-engines
            (list (make-instance 'search-engine
                                 :shortcut "default"
                                 :search-url "https://qwant.com/?q=~a"
                                 :fallback-url "https://qwant.com/")
                  (make-instance 'search-engine
                                 :shortcut "qw"
                                 :search-url "https://qwant.com/?q=~a"
                                 :fallback-url "https://qwant.com/")
                  (make-instance 'search-engine
                                 :shortcut "gh"
                                 :search-url "https://github.com/?q=~a"
                                 :fallback-url "https://github.com/")
                  ))
           (autofills (list (nyxt::make-autofill :key "Name" :fill "${secrets.name}")))
           ))
        (setq *configured-browser* t))
    '';
    assertions = [
      {
        message = "Nyxt config:";
        assertion = true;
      }
    ];
  };
}

## Local Variables: ***
## mode: nix-dsquoted-commonlisp ***
## End: ***
