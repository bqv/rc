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
      ;(setq browse-url-browser-function 'webkit-browse-url)
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

      (defun webkit-kill-buffer-query-function ()
        (not (eq major-mode 'webkit-mode)))
      (advice-add #'process-kill-buffer-query-function :before-while #'webkit-kill-buffer-query-function)
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
