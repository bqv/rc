;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . (
        ;(eval . (set (make-local-variable 'default-directory)
        ;             (locate-dominating-file buffer-file-name ".dir-locals.el")))
         (compile-command . "env TERM=dumb nix run -vv '.#delta' --show-trace")
         (eval . (setq projectile-project-compilation-cmd
                       "env TERM=dumb nix build -vv --show-trace && ./result && git push"))
         )))
