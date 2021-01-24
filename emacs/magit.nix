{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.magit = {
    demand = true;
    bind = {
      "C-x g" = "magit-status";
      "C-x M-g" = "magit-dispatch-popup";
      "C-c g" = "magit-dispatch";
    };
    config = ''
      (setq magit-log-show-refname-after-summary t)
      (setq magit-clone-default-directory (expand-file-name "~/dev/"))
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)
      (setq magit-section-initial-visibility-alist nil)
      (setq magit-wip-merge-branch t)
      (magit-wip-mode)

      ; temp fix
      (defun server-switch-buffer--with-editor-server-window-alist
          (fn &optional next-buffer &rest args)
        "Honor `with-editor-server-window-alist' (which see)."
        (let ((server-window (with-current-buffer
                                 (or next-buffer (current-buffer))
                               (when with-editor-mode
                                 (setq with-editor-previous-winconf
                                       (current-window-configuration)))
                               (with-editor-server-window))))
          (apply fn next-buffer args)))

      (defun magit-process-filter (proc string)
        "Default filter used by `magit-start-process'."
        (with-current-buffer (process-buffer proc)
          (let ((inhibit-read-only t))
            (magit-process-yes-or-no-prompt proc string)
            (magit-process-username-prompt  proc string)
            (magit-process-password-prompt  proc string)
            (goto-char (process-mark proc))
            (setq string (propertize string 'magit-section
                                     (process-get proc 'section)))
            ;; Find last ^M in string.  If one was found, ignore
            ;; everything before it and delete the current line.
            (let ((ret-pos (length string)))
              (while (and (>= (cl-decf ret-pos) 0)
                          (/= ?\r (aref string ret-pos))))
              (if (< ret-pos 0)
                  (insert string)
                (delete-region (line-beginning-position) (point))
                (insert (substring string (1+ ret-pos)))))
            (let ((original-end (point)))
              (goto-char (process-mark proc))
              (let ((escapes nil)
                    (first-escape nil)
                    (current-escape nil)
                    (text-start nil))
                (while (re-search-forward
                        (rx "[" (? (char ??)) (? (group (1+ digit)))
                            (group (char ?A ?B ?D ?K ?l ?h)))
                        nil t)
                  (setq current-escape (match-beginning 0))
                  (unless first-escape (setq first-escape current-escape))
                  (push (match-string-no-properties 0) escapes)
                  (setq text-start (point))
                  (re-search-forward (rx (group (*? any)) (or "[" buffer-end)) nil t)
                  (if (save-match-data (looking-back "\\[" (- (point) 4))) (backward-char 2))
                  (push (buffer-substring-no-properties text-start (point)) escapes)
                  (delete-region current-escape (point)))
                (setq escapes (nreverse escapes))
                (if first-escape
                    (progn
                      (goto-char first-escape)
                      (delete-region (point) (point-max))
                      (while escapes
                        (let ((current (car escapes)))
                          (if (string-match (rx "["
                                                (? (char ??))
                                                (? (group (1+ digit)))
                                                (group (char ?A ?B ?D ?K ?l ?h)))
                                            current)
                              (let ((arg (ignore-errors (string-to-number (match-string 1 current))))
                                    (command (match-string 2 current)))
                                (cond
                                 ((equal command "A")
                                  (forward-line (- arg)))
                                 ((equal command "B")
                                  (forward-line arg))
                                 ((equal command "D")
                                  (let ((lb (line-beginning-position)))
                                    (if (< lb (- (point) arg))
                                        (backward-char arg)
                                      (beginning-of-line))))
                                 ((equal command "K")
                                  (cond
                                   ((or (not arg) (= arg 0))
                                    (delete-region (point) (min (point-max) (1+ (line-end-position)))))
                                   ((= arg 1)
                                    (let ((replace-region (delete-and-extract-region (line-beginning-position) (point))))
                                      (insert (make-string (length replace-region) 32))))
                                   ((= arg 2)
                                    (let ((replace-region (delete-and-extract-region (line-beginning-position) (point))))
                                      (delete-region (point) (line-end-position))
                                      (insert (make-string (length replace-region) 32))))))))
                            (insert current))
                          (!cdr escapes))))
                  (goto-char original-end)))
              (set-marker (process-mark proc) (point))))))
    '';
    systemDeps = with pkgs; [ git ];
  };
}
