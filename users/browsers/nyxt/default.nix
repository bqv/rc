{ config, lib, pkgs, ... }:

{
  config = {
    home.file.".config/nyxt/init.lisp".force = true;
    home.file.".config/nyxt/init.lisp".text = let
      secrets = import ../../../secrets/nyxt.autofill.nix;
    in ''
      (setq *swank-port* 4005)
      (handler-case
        (start-swank)
        (uiop:launch-program (list "emacsclient" "--eval" "(nyxt-repl)"))
        (error () nil))

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

      (define-command clear-shell (shell-mode)
        "Clear the output in the shell buffer."
        (ipc-buffer-evaluate-javascript
         (active-buffer *browser*)
         (clear-shell-output)))

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

      (define-configuration buffer
        ((keymap-scheme-name scheme:emacs)
         (default-new-buffer-url "about:blank")
         (override-map (let ((map (make-keymap "override-map")))
                                   (define-key map
                                     "M-x" 'execute-command
                                     "C-x C-c" 'quit)
                                  ;(define-key map "C-x s" 'shell)
                                   (define-key map "C-r" 'reload-page)
                                   (define-key map
                                     "C-v" 'scroll-page-down
                                     "M-v" 'scroll-page-up)
                                   map))
         ))

      (define-configuration browser
        ((session-restore-prompt :always-restore)
         (external-editor-program "gnvim")
         (download-path (make-instance 'download-data-path :dirname "~/tmp"))
         (start-page-url "https://nyxt.atlas.engineer/quickstart")
         (search-engines (list (make-instance 'search-engine
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
