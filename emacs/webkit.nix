{ config, lib, usr, pkgs, domains, ... }:

{
  emacs-loader.webkit = let
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
      ];
    };
  in {
    demand = true;
    package = epkgs: epkgs.emacs-webkit;
    bind = {
      "M-*" = "webkit";
    };
    init = ''
      (setenv "GIO_EXTRA_MODULES"
              (let ((cur (getenv "GIO_EXTRA_MODULES"))
                    (new "${pkgs.glib-networking}/lib/gio/modules"))
                (if cur (concat new ":" cur) new)))
      (setenv "GST_PLUGIN_SYSTEM_PATH_1_0"
              (let ((cur (getenv "GST_PLUGIN_SYSTEM_PATH_1_0"))
                    (new "${pkgs.glib-networking}/lib/gio/modules"))
                (if cur (concat new ":" cur) new)))
    '';
    config = ''
      (with-eval-after-load 'evil
        (require 'evil-collection-webkit)
        (evil-collection-xwidget-setup))
      (setq webkit-own-window nil)
      (setq webkit-search-prefix "https://qwant.com/?q=")
    '';
  };
}
