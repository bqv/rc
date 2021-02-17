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
        "${pkgs.pulseeffects}/lib/gstreamer-1.0"
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
      (setq browse-url-browser-function #'webkit-browse-url)
      (setq browse-url-secondary-browser-function #'browse-url-generic)
      (setq webkit-browse-url-force-new t)

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

      ;;
      (defun flycheck-handle-buffer-switch ()
  "Handle a possible switch to another buffer.

If a buffer switch actually happened, schedule a syntax check."
  ;; Switching buffers here is weird, but unfortunately necessary.  It
  ;; turns out that `with-temp-buffer' triggers
  ;; `buffer-list-update-hook' twice, and the value of
  ;; `current-buffer' is bogus in one of those triggers (the one just
  ;; after the temp buffer is killed).  If we rely on the bogus value,
  ;; Flycheck will think that the user is switching back and forth
  ;; between different buffers during the `with-temp-buffer' call
  ;; (note: two different normal buffers, not the current buffer and
  ;; the temp buffer!), and that would trigger spurious syntax checks.
  ;; It seems that reading (window-buffer) gets us the correct current
  ;; buffer in all important real-life situations (although it doesn't
  ;; necessarily catch uses of `set-buffer').
  (with-current-buffer (window-buffer)
    (unless (or (equal flycheck--last-buffer (current-buffer))
                ;; Don't bother keeping track of changes to and from
                ;; the minibuffer, as they will never require us to
                ;; run a syntax check.
                (minibufferp))
      (setq flycheck--last-buffer (current-buffer))
      (when (and flycheck-mode
                 (memq 'idle-buffer-switch flycheck-check-syntax-automatically))
        (flycheck--clear-idle-trigger-timer)
        (cl-pushnew 'idle-buffer-switch flycheck--idle-trigger-conditions)
        (setq flycheck--idle-trigger-timer
              (run-at-time flycheck-idle-buffer-switch-delay nil
                           #'flycheck--handle-idle-trigger
                           (current-buffer)))))))

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
}
