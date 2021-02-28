{ config, lib, usr, pkgs, hosts, ... }:

{
  emacs.loader.vterm = {
    demand = true;
    config = ''
      (setq vterm-shell "${pkgs.xonsh.pname}")
      (setq vterm-kill-buffer-on-exit t)
      (defun vterm-shell-command (command)
        (let ((vterm-shell command))
          (vterm "*Vterm Shell Command*")))
      (defun bqv/nested-emacs ()
        (interactive)
        (vterm-shell-command "*nested-emacs*"))

      (defun vterm-run (with-sudo executable)
        "Launch EXE in a vterm buffer, possibly WITH-SUDO."
        ;(interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell (if with-sudo
                                 (concat "sudo " executable)
                                 executable)))
            (vterm buffer-name))))
      (defun htop (with-sudo)
        (interactive "P")
        (vterm-run with-sudo "htop"))
      (defun tuir (with-sudo)
        (interactive "P")
        (vterm-run with-sudo "tuir"))
    '';
    systemDeps = with pkgs; [ cmake libtool libvterm ];
  };
}
