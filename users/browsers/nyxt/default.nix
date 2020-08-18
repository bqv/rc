{ super, config, lib, pkgs, ... }:

{
  config = {
    home.file.".config/nyxt/init.lisp".force = true;
    home.file.".config/nyxt/init.lisp".text = let
      secrets = import ../../../secrets/nyxt.autofill.nix;
    in ''
      (setq *swank-port* 4005)
      (ignore-errors
       (start-swank)
       (uiop:launch-program (list "emacsclient" "--eval" "(nyxt-repl)"))
                            (list "emacsclient" "--eval" "(setq slime-enable-evaluate-in-emacs t)"))

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
         (default-modes (cons 'blocker-mode %slot-default))
         (request-resource-hook
          (reduce #'hooks:add-hook
                  (list ;; doi://path -> https url
                        (url-dispatching-handler
                         'doi-link-dispatcher (match-scheme "doi")
                         (lambda (url)
                           (quri:uri (format nil "https://doi.org/~a"
                                             (quri:uri-path url)))))

                        ;; github://owner/repo -> https url
                        (url-dispatching-handler
                         'doi-link-dispatcher (match-scheme "github" "gh")
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
                                 ;"--no-cache" "--cache-secs=0" "--demuxer-readahead-secs=0" "--untimed"
                                 ;"--cache-pause=no"
                                  (object-string url)))
                            nil))
                  )
                  :initial-value %slot-default))
         ))

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
    '';
  };
}

## Local Variables: ***
## mode: nix-dsquoted-commonlisp ***
## End: ***
