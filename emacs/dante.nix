{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.dante = {
    demand = true;
    after = [ "haskell-mode" ];
    hook = [
      { haskell-mode-hook = "dante-mode"; }
    ];
    config = ''
      (add-to-list 'haskell-mode-hook #'dante-mode)
      (defun dante-setup-flake ()
        (interactive)
        (setq dante-repl-command-line '(
          "nix" "develop" "--impure" "-c" "cabal" "v1-repl"
          (or dante-target (dante-package-name) #1="")
          "--builddir=dist/dante")))
    '';
  };
}
