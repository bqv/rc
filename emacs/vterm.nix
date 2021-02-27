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

      (defun vterm--run (command)
        "Launch COMMAND in a vterm buffer."
        ;(interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((executable (car (split-string command " ")))
               (buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell command))
            (vterm buffer-name))))
      (defun vterm--run-sudo (command)
        "Launch sudo COMMAND in a vterm buffer."
        ;(interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((executable (car (split-string command " ")))
               (buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell (concat "sudo " command)))
            (vterm buffer-name))))
      (defun vterm-run (with-sudo &rest exe)
        "Launch EXE in a vterm buffer, possibly WITH-SUDO."
        ;(interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((executable (car (split-string command " ")))
               (buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell (concat "sudo " command)))
            (vterm buffer-name))))
        ;(interactive "P")
        (if with-sudo
          (apply #'call-interactively (cons #'vterm--run-sudo EXE))
          (apply #'call-interactively (cons #'vterm--run EXE))))
      (defun htop (with-sudo)
        (interactive "P")
        (if with-sudo
          (vterm--run-sudo "htop")
          (vterm--run "htop")))
      (defun tuir ()
        (interactive)
        (vterm--run "tuir"))
    '';
    systemDeps = with pkgs; [ cmake libtool libvterm ];
  };
}
