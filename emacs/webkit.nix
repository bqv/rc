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
      (defun webkit-history-completion-text (item)
        (let* ((title (webkit-history-item-title item))
               (uri (webkit-history-item-uri item))
               (visit-count (webkit-history-item-visit-count item))
               (last-time (webkit-history-item-last-time item))
               (text (concat title " (" uri ")")))
          (put-text-property (+ 2 (length title)) (1- (length text)) 'face 'link text)
          (propertize text
                      'webkit-title title
                      'webkit-uri uri
                      'webkit-visit-count visit-count
                      'webkit-last-time last-time)))

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
          (let* ((completion (ivy-read prompt completions
                                       :caller 'webkit-history-completing-read))
                 (uri (cdr (assoc completion completions))))
            (if uri uri completion))))

      (defun ivy-rich-webkit-history-title (candidate)
        (let* ((data (text-properties-at 0 candidate))
               (value (plist-get data 'webkit-title)))
          (if value value "?")))

      (defun ivy-rich-webkit-history-uri (candidate)
        (let* ((data (text-properties-at 0 candidate))
               (value (plist-get data 'webkit-uri)))
          (if value value "?")))

      (defun ivy-rich-webkit-history-visit-count (candidate)
        (let* ((data (text-properties-at 0 candidate))
               (value (plist-get data 'webkit-visit-count)))
          (if value (format "% 5d times" value) "?")))

      (defun ivy-rich-webkit-history-last-time (candidate)
        (let* ((data (text-properties-at 0 candidate))
               (value (plist-get data 'webkit-last-time)))
          (if value (format-time-string "%Y-%m-%d %H:%M:%S" value) "?")))

      (add-to-list ivy-rich-display-transformers-list
                   'webkit-history-completing-read
                   t #'ignore)
      (add-to-list ivy-rich-display-transformers-list
                   '((ivy-rich-webkit-history-title (:width 0.4))
                     (ivy-rich-webkit-history-uri (:width 0.4))
                     (ivy-rich-webkit-history-last-time (:face font-lock-comment-face))
                     (ivy-rich-webkit-history-visit-count (:align right)))
                   t #'ignore)
      (memq
       'webkit-history-completing-read
       ivy-rich-display-transformers-list)
    '';
  };
}
