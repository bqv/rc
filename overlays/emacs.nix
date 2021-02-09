inputs@{...}: final: prev: let
  emacsOverride = self: super: {
    weechat-patched = super.weechat.overrideAttrs (o: {
      patches = (o.patches or []) ++ [
        (final.fetchpatch {
          name = "rx-form_to_rx--translate-form.patch";
          url = "https://github.com/emacsomancer/weechat.el/commit/77f3e93fabcf300ba6c2d268e2e89baabed6cbf3.patch";
          sha256 = "0j1f6vmmk3sk1mvdwnxwbk5sbs1cdpqw1drlg53z7xr529d7f0bf";
        })
        (final.fetchpatch {
          name = "reversed_rx_character_range.patch";
          url = "https://github.com/eklitzke/weechat.el/commit/446868de424170be0584980d4dcc0859f7077d54.patch";
          sha256 = "1q40g3kdq8418dbi9z6i9v3wj4wbh3k3yn77lvyk1yp14jk696qx";
        })
        (final.fetchpatch {
          name = "ellipsize_nicks.patch";
          url = "https://github.com/bqv/weechat.el/commit/6f655866206eac179f74637c77e6c4b259f9891c.patch";
          sha256 = "1bg8kxq7cpgyywmadnc2z86jb0ib10d5s3zwlxynvwsbs39xcby0";
        })
        (final.fetchpatch {
          name = "reverse_speedbar_order.patch";
          url = "https://github.com/bqv/weechat.el/commit/2c572326c26d1f62bce2926b3cb00c7c2e971406.patch";
          sha256 = "1xncc6212981fp7fm3381l95h397m8f4li0ksv236a8bmpl62vd3";
        })
      ];
    });
  };

  wrapGApps = drv: rec {
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [
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
    ] ++ gstBuildInputs ++ (drv.buildInputs or []);

    GIO_EXTRA_MODULES = "${final.glib-networking}/lib/gio/modules:${final.dconf.lib}/lib/gio/modules";
    GST_PLUGIN_SYSTEM_PATH_1_0 = final.lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs;
    dontWrapGApps = true;
    installPhase = drv.installPhase + ''
      makeWrapper $src/bin/emacsclient $out/bin/emacsclient \
        --prefix LD_LIBRARY_PATH : "${final.lib.makeLibraryPath buildInputs}" \
        "''${gappsWrapperArgs[@]}" \
        --argv0 emacsclient
    '';
  };
in with prev.lib; rec {
  emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope' emacsOverride;

  emacsGcc = prev.emacsGcc.overrideAttrs (drv: wrapGApps drv // {
    passthru = drv.passthru // { nativeComp = true; };
  });

  emacsPgtkGcc = prev.emacsPgtkGcc.overrideAttrs (drv: wrapGApps drv // {
    passthru = drv.passthru // {
      nativeComp = true;
      pkgs = final.emacsPackagesFor final.emacsPgtkGcc;
    };
  });

  emacsPgtkGccClient = final.stdenv.mkDerivation rec {
    pname = "emacsclient";
    inherit (src) version;
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
      makeWrapper $src/bin/emacsclient $out/bin/emacsclient \
        --prefix LD_LIBRARY_PATH : "${final.lib.makeLibraryPath buildInputs}" \
        "''${gappsWrapperArgs[@]}" \
        --argv0 emacsclient
    '';
  };

  emacsPgtkGccPackages = final.lib.dontRecurseIntoAttrs (final.emacsPackagesFor final.emacsPgtkGcc);
}
