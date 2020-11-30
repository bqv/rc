inputs@{...}: final: prev: rec {
  emacsPgtkGccClient = final.stdenv.mkDerivation rec {
    src = final.emacsPgtkGcc;
    dontBuild = true;
    nativeBuildInputs = [
      final.wrapGAppsHook
    ];
    gstBuildInputs = with final; with gst_all_1; [
      gstreamer gst-libav
      gst-plugins-base
      gst-plugins-good
      gst-plugins-bad
      gst-plugins-ugly
    ];
    buildInputs = with final; [
      webkitgtk
      glib gdk-pixbuf cairo
      mime-types pango gtk3
      glib-networking gsettings-desktop-schemas
      xclip notify-osd enchant
    ] ++ gstBuildInputs;

    GIO_EXTRA_MODULES = "${final.glib-networking}/lib/gio/modules:${final.dconf.lib}/lib/gio/modules";
    GST_PLUGIN_SYSTEM_PATH_1_0 = final.lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs;
    dontWrapGApps = true;
    installPhase = ''
      makeWrapper $src/bin/emacsclient "''${gappsWrapperArgs[@]}" \
        --prefix LD_LIBRARY_PATH : "${final.lib.makeLibraryPath buildInputs}" \
        --argv0 emacsclient
    '';
  };

  emacsPgtkGccPackages = final.lib.dontRecurseIntoAttrs (final.emacsPackagesFor final.emacsPgtkGcc);

  # Overridden for exwm
  emacsWithPackages = emacsPgtkGccPackages.emacsWithPackages;
}
