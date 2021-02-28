{ config, lib, usr, pkgs, hosts, ... }:

{
  emacs.loader.vterm = {
    demand = true;
    config = ''
      (setq vterm-shell "${pkgs.xonsh.pname}")
      (setq vterm-kill-buffer-on-exit t)
      (defun bqv/nested-emacs ()
        (interactive)
        (let ((vterm-shell "emacs -nw"))
          (vterm "*nested-emacs*")))
      (defun vterm-shell-command (command)
        (let ((vterm-shell command))
          (vterm "*Vterm Shell Command*")))

      (defun vterm-run (with-sudo executable)
        "Launch EXE in a vterm buffer, possibly WITH-SUDO."
        ;(interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell (if with-sudo
                                 (concat "sudo " exe)
                                 exe)))
            (vterm buffer-name))))
      (defun htop (with-sudo)
        (interactive "P")
        (vterm-run with-sudo "htop"))
      (defun tuir (with-sudo)
        (interactive)
        (vterm-run with-sudo "tuir"))
    '';
    systemDeps = with pkgs; [ cmake libtool libvterm ];
  };
}
