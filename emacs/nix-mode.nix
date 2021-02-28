{ config, lib, usr, pkgs, flake, ... }:

{
  emacs.loader.nix-mode = let
    inputs = { self = flake; } // flake.inputs;
  in {
    demand = true;
    config = ''
      (setq nix-repl-executable-args '("-vv" "repl" "--impure" "--builders" ""
                                       "--option" "allow-unsafe-native-code-during-evaluation" "true"
                                       "/run/current-system/flake/input/self/"))
      (setq nix-indent-function 'nix-indent-line)

      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: path: ''
        (defun flake-${name}-rg ()
          (interactive)
          (counsel-rg nil "/run/current-system/flake/input/${name}/" nil "[flake:${name}] rg: ")) '') inputs)}

      (add-to-list 'nix-repl-mode-hook 'company-mode)
      (define-key nix-repl-mode-map (kbd "<tab>") #'company-complete)
      (define-key nix-repl-mode-map (kbd "C-i") #'company-complete)

      (defun nixos-configuration-help ()
        (interactive)
        (vterm-shell-command "env PAGER=less man configuration.nix"))

      (defun home-configuration-help ()
        (interactive)
        (vterm-shell-command "env PAGER=less man home-configuration.nix"))

      (defmacro defcmd (name body &rest cdr)
        `(defun ,name () (interactive) ,body ,@cdr))

      (defun unwords (ss)
        (s-join " " ss))

      (setq nixos-running-p nil)
      (defun nixos-sentinel (process signal)
        (when (memq (process-status process) '(exit signal))
          (message "NixOS job finished!")
          (setq check-nixos t)
          (setq nixos-running-p nil)
          (shell-command-sentinel process signal)))

      (defun nixos (cmd)
        (let* ((output-buffer "*nixos*")
               (proc (progn
                       (setq nixos-running-p t)
                       (message (s-lex-format "Starting NixOS job: ''${cmd}"))
                       (async-shell-command cmd output-buffer)
                       (get-buffer-process output-buffer))))
          (if (process-live-p proc)
              (set-process-sentinel proc #'nixos-sentinel)
            (message "No NixOS process running."))))

      (defun nixos-with-args (cmd)
        (nixos (unwords (cons cmd (transient-args 'nixos-dispatch)))))

      (defcmd nixos-garbage-collect
        (nixos-with-args "doas nix-collect-garbage"))

      (defcmd nixos-rebuild
        (nixos-with-args "doas nix run /srv/git/github.com/bqv/nixrc/"))

      (defcmd nixos-check-vulnerabilities
        (nixos "doas vulnix --system"))

      (define-infix-argument nixos:--delete-older-than ()
        :description "delete older than"
        :class 'transient-option
        :shortarg "-d"
        :argument "--delete-older-than ")

      (define-infix-argument nixos:--tarball-ttl ()
        :description "tarball cache lifetime (seconds)"
        :class 'transient-option
        :shortarg "-t"
        :argument "--option tarball-ttl ")

      (defcmd nix-store-verify
        (nixos "doas nix-store --verify --check-contents"))

      (defcmd nix-store-repair
        (nixos "doas nix-store --verify --check-contents --repair"))

      (defcmd nix-store-optimize
        (nixos "doas nix-store --optimise -vv"))

      (defun nix-store-query-dependents (path)
        (interactive "sPath: ")
        (nixos (s-lex-format "doas nix-store --query --roots ''${path}")))

      (defun nix-store-delete (path)
        (interactive "sPath: ")
        (nixos (s-lex-format "doas nix-store --delete ''${path}")))

      (defun nixos-search-options (option)
        (interactive "sOption: ")
        (browse-url (s-lex-format "https://nixos.org/nixos/options.html#''${option}")))

      (defun nixos-search-packages (query)
        (interactive "sPackage: ")
        (browse-url (s-lex-format "https://nixos.org/nixos/packages.html?query=''${query}")))

      (defun nixos-howoldis ()
        (interactive)
        (browse-url "https://howoldis.herokuapp.com/"))

      (defun nixos-index ()
        (interactive)
        (nixos "doas nix-index"))

      (define-transient-command nixos-garbage-collect-dispatch ()
        ["Garbage collection"
         (nixos:--delete-older-than)
         ("g" "collect garbage" nixos-garbage-collect)])

      (define-transient-command nixos-dispatch ()
        ["Arguments"
         ("-t" "show trace" "--show-trace")
         (nixos:--tarball-ttl)]
        ["NixOS"
         ("r" "rebuild" nixos-rebuild)
         ("o" "search options" nixos-search-options)
         ("p" "search packages" nixos-search-packages)
         ("h" "check channels" nixos-howoldis)
         ("i" "index" nixos-index)]
        ["Garbage collection"
         ("g" "collect garbage" nixos-garbage-collect-dispatch)]
        ["Nix Store"
         ("s v" "verify" nix-store-verify)
         ("s r" "repair" nix-store-repair)
         ("s d" "query dependents" nix-store-query-dependents)
         ("s k" "delete" nix-store-delete)
         ("s o" "optimize" nix-store-optimize)])

      (define-key global-map (kbd "C-c n") #'nixos-dispatch)
    '';
  };
}
