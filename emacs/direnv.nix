{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.direnv = {
    enable = false;
    demand = true;
    config = ''
      (direnv-mode 1)
      (defcustom bqv/direnv-enabled-hosts nil
        "List of remote hosts to use Direnv on.
      Each host must have `direnv' executable accessible in the default environment."
        :type '(repeat string)
        :group 'bqv)

      (defun bqv/advice-filter@tramp-sh-handle-start-file-process (args)
        "Enable Direnv for hosts in `my-direnv-enabled-hosts'."
        (with-parsed-tramp-file-name (expand-file-name default-directory) nil
          (if (member host bqv/direnv-enabled-hosts)
              (pcase-let ((`(,name ,buffer ,program . ,args) args))
                `(,name
                  ,buffer
                  "direnv"
                  "exec"
                  ,localname
                  ,program
                  ,@args))
            args)))

      (with-eval-after-load "tramp-sh"
        (advice-add 'tramp-sh-handle-start-file-process
                    :filter-args #'bqv/advice-filter@tramp-sh-handle-start-file-process))
    '';
    systemDeps = with pkgs; [ direnv ];
  };
}
