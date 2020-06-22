;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . (
        ;(eval . (set (make-local-variable 'default-directory)
        ;             (locate-dominating-file buffer-file-name ".dir-locals.el")))
         (compile-command . "nixos -Tvbas")
         (eval . (setq projectile-project-compilation-cmd
                       "nixos -Tvbas"))
         )))
