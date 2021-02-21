{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.helpful = {
    demand = true;
    config = ''
      nil
      ;; Note that the built-in `describe-function' includes both functions
      ;; and macros. `helpful-function' is functions only, so we provide
      ;; `helpful-callable' as a drop-in replacement.
      (global-set-key (kbd "C-h f") #'helpful-callable)

      (global-set-key (kbd "C-h v") #'helpful-variable)
      (global-set-key (kbd "C-h k") #'helpful-key)

      ;; Lookup the current symbol at point. C-c C-d is a common keybinding
      ;; for this in lisp modes.
      (global-set-key (kbd "C-c C-d") #'helpful-at-point)

      ;; Look up *F*unctions (excludes macros).
      ;;
      ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
      ;; already links to the manual, if a function is referenced there.
      (global-set-key (kbd "C-h F") #'helpful-function)

      ;; Look up *C*ommands.
      ;;
      ;; By default, C-h C is bound to describe `describe-coding-system'. I
      ;; don't find this very useful, but it's frequently useful to only
      ;; look at interactive functions.
      (global-set-key (kbd "C-h C") #'helpful-command)

      (setq counsel-describe-function-function #'helpful-callable)
      (setq counsel-describe-variable-function #'helpful-variable)
    '';
  };
}
