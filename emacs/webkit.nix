{ config, lib, usr, pkgs, domains, ... }:

{
  emacs.loader.webkit = let
    env = {
      GIO_EXTRA_MODULES = with pkgs; [
        "${glib-networking}/lib/gio/modules"
      ];
      GST_PLUGIN_SYSTEM_PATH_1_0 = with pkgs.gst_all_1; [
        "${gstreamer}/lib/gstreamer-1.0"
        "${gst-libav}/lib/gstreamer-1.0"
        "${gst-plugins-base}/lib/gstreamer-1.0"
        "${gst-plugins-good}/lib/gstreamer-1.0"
        "${gst-plugins-bad}/lib/gstreamer-1.0"
        "${gst-plugins-ugly}/lib/gstreamer-1.0"
        "${pkgs.pipewire.lib}/lib/gstreamer-1.0"
        "${pkgs.pulseeffects-pw}/lib/gstreamer-1.0"
      ];
    };
  in {
    demand = true;
    package = epkgs: epkgs.emacs-webkit;
    bind = {
      "M-*" = "webkit";
    };
    init = lib.concatMapStringsSep "" ({ name, value }: ''
      (setenv "${name}"
              (let ((cur (getenv "${name}"))
                    (new "${lib.concatStringsSep ":" value}"))
                (if cur (concat new ":" cur) new)))
    '') (lib.mapAttrsToList lib.nameValuePair env);
    config = ''
      (with-eval-after-load 'evil-collection
        (require 'evil-collection-webkit)
        (evil-collection-xwidget-setup))
      (setq webkit-own-window nil)
      (setq webkit-search-prefix "https://qwant.com/?q=")
      (setq webkit-browse-url-force-new t)

      (add-hook 'after-init-hook
                (lambda (&rest _)
                  (setq browse-url-secondary-browser-function browse-url-browser-function)
                  (setq browse-url-browser-function #'webkit-browse-url)))

      (with-eval-after-load 'all-the-icons
        (defun webkit--display-progress (progress)
          (setq webkit--progress-formatted
                (if (equal progress 100.0)
                    ""
                  (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
          (force-mode-line-update)))

      ;; save function saves to the download directory, open function
      ;; opens in a temp buffer and default function interactively prompts.
      (setq webkit-download-action-alist '(("\\.pdf\\'" . webkit-download-open)
                                           ("\\.png\\'" . webkit-download-save)
                                           (".*" . webkit-download-default)))

      ;; no "running process" prompt for webkit buffers
      (defun webkit-process-kill-buffer-advice (orig-fun)
        (or (eq major-mode 'webkit-mode) (funcall orig-fun)))
      (advice-add #'process-kill-buffer-query-function :around #'webkit-process-kill-buffer-advice)

      ;; fix evil not exiting consistently on unfocus
      (defun webkit-handle-buffer-switch ()
        "Handle a possible switch to another buffer."
        (let ((new-buffer (window-buffer))
              (old-buffer (current-buffer)))
          (when (and (eq major-mode 'webkit-mode)
                     (featurep 'evil-collection-webkit))
            (evil-collection-webkit-unfocus-to-normal-mode old-buffer))))
      (add-to-list 'buffer-list-update-hook #'webkit-handle-buffer-switch)

      (with-eval-after-load 'emms
        (defun webkit-play-url (&optional webkit-id)
          (interactive)
          (let ((uri (webkit--get-uri (or webkit-id webkit--id))))
            (message "Playing %s" uri)
            (emms-play-url uri)))
        (define-key webkit-mode-map (kbd "C-c u") 'webkit-play-url)
        (with-eval-after-load 'evil-collection-webkit
          (evil-collection-define-key 'normal 'webkit-mode-map
            "U" 'webkit-play-url)))
    '';
  };
  emacs.loader.webkit-ace = {
    demand = true;
    inherit (config.emacs.loader.webkit) package;
    after = [ "webkit" ];
  };
  emacs.loader.webkit-dark = {
    demand = true;
    inherit (config.emacs.loader.webkit) package;
    after = [ "webkit" ];
    config = ''
      (setq webkit-dark-mode nil)
    '';
  };
  emacs.loader.webkit-history = {
    demand = true;
    inherit (config.emacs.loader.webkit) package;
    after = [ "webkit" ];
    config = ''
      (defun webkit-history-completing-read (prompt)
        "Prompt for a URI using COMPLETING-READ from webkit history."
        (let ((completions ())
              (key-to-count (lambda (k) (webkit-history-item-visit-count
                                         (gethash (cdr k) webkit-history-table))))
              (key-to-time (lambda (k) (webkit-history-item-last-time
                                        (gethash (cdr k) webkit-history-table)))))
          (maphash (lambda (k v)
                     (push (cons (webkit-history-completion-text v) k) completions))
                   webkit-history-table)
          (setq completions (sort completions (lambda (k1 k2)
                                                (let ((c1 (funcall key-to-count k1))
                                                      (c2 (funcall key-to-count k2))
                                                      (t1 (funcall key-to-time k1))
                                                      (t2 (funcall key-to-time k2)))
                                                  (if (= c1 c2)
                                                      (> t1 t2)
                                                    (> c1 c2))))))
          (let* ((completion (completing-read prompt completions))
                 (uri (cdr (assoc completion completions))))
            (if uri uri completion))))

      (defun ivy-rich-webkit-last-visited-time (candidate)
        (let ((candidate (expand-file-name candidate ivy--directory)))
          (if (or (file-remote-p candidate) (not (file-exists-p candidate)))
              (progn (print candidate) "?")
            (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 (file-attributes candidate))))))

      ((webkit-history-completing-read (:columns ((ivy-rich-candidate (:width 0.8)) (ivy-rich-webkit-last-visited-time (:face font-lock-comment-face)))))
       ivy-switch-buffer
       (:columns ((ivy-switch-buffer-transformer (:width 0.35)) (ivy-rich-switch-buffer-size (:width 7)) (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)) (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)) (ivy-rich-switch-buffer-project (:width 0.18 :face success)) (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))) :predicate (lambda (cand) (get-buffer cand)))
       counsel-find-file
       (:columns ((ivy-read-file-transformer) (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
       counsel-M-x
       (:columns ((counsel-M-x-transformer (:width 0.4)) (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-function
       (:columns ((counsel-describe-function-transformer (:width 0.4)) (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-variable
       (:columns ((counsel-describe-variable-transformer (:width 0.4)) (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
       counsel-recentf
       (:columns ((ivy-rich-candidate (:width 0.8)) (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
       counsel-bookmark
       (:columns ((ivy-rich-candidate (:width 0.3)) (ivy-rich-bookmark-type) (ivy-rich-bookmark-info)))
       package-install
       (:columns ((ivy-rich-candidate (:width 30)) (ivy-rich-package-version (:width 16 :face font-lock-comment-face)) (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face)) (ivy-rich-package-install-summary (:face font-lock-doc-face)))))
      (add-to-list 'ivy-rich-display-transformers-list
        '(webkit-history-completing-read
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-webkit-last-visited-time (:face font-lock-comment-face))))))
    '';
  };
}
