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
      (defun vterm-run (with-sudo &rest r)
        ;(interactive "P")
        (if with-sudo
          (apply #'call-interactively (cons #'vterm--run-sudo r))
          (apply #'call-interactively (cons #'vterm--run r))))
      (defun vterm--run (command)
        "Launch COMMAND in a vterm buffer."
        (interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((executable (car (split-string command " ")))
               (buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell command))
            (vterm buffer-name))))
      (defun vterm-shell-command (command)
        ;(interactive "sVTerm shell command: ")
        (let ((vterm-shell command))
          (vterm "*vterm-shell-command*")))
      (global-set-key (kbd "C-x M-&") #'vterm-shell-command)
      (defun vterm--run-sudo (command)
        "Launch sudo COMMAND in a vterm buffer."
        (interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((executable (car (split-string command " ")))
               (buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell (concat "sudo " command)))
            (vterm buffer-name))))
      (defun vterm-ssh-run (with-sudo &rest r)
        (interactive "P")
        (if with-sudo
          (apply #'call-interactively (cons #'vterm--ssh-run-sudo r))
          (apply #'call-interactively (cons #'vterm--ssh-run r))))
      (defun vterm--ssh-run (server command)
        "Launch COMMAND over ssh to SERVER, in a vterm buffer."
        (interactive (list (completing-read "Server" '("${hosts.wireguard.ipv4.zeta}"))
                           (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((executable (car (split-string command " ")))
               (buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell (concat "ssh " server " -t " command)))
            (vterm buffer-name))))
      (defun vterm--ssh-run-sudo (server command)
        "Launch sudo COMMAND over ssh to SERVER, in a vterm buffer."
        (interactive (list (completing-read "Server" '("${hosts.wireguard.ipv4.zeta}"))
                           (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
        (let* ((executable (car (split-string command " ")))
               (buffer-name (concat "*" executable "*"))
               (canonical-name (assoc executable (executables-list)
                                      '(lambda (exe) (= (file-name-base exe) )))))
          (assert (not (null executable)))
          (let ((vterm-shell (concat "ssh " server " -t sudo " command)))
            (vterm buffer-name))))
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
