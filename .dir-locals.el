;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . (
        ;(eval . (set (make-local-variable 'default-directory)
        ;             (locate-dominating-file buffer-file-name ".dir-locals.el")))
         (compile-command . "systemd-run --user --scope nix run . -- -Tbasv")
         (eval . (setq projectile-project-compilation-cmd
                       "systemd-run --user -u nixos --scope if nix build '' ./result"))
         )))
