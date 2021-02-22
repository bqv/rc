inputs@{...}: final: prev: let
  emacsOverride = self: super: {
    weechat-patched = super.weechat.overrideAttrs (o: {
      patches = (o.patches or []) ++ map ({ name, rev, hash }: final.fetchpatch {
        name = "${name}.patch";
        url = "https://github.com/bqv/weechat.el/commit/${rev}.patch";
        sha256 = hash;
      }) [
        {
          name = "rx-form_to_rx--translate-form";
          rev = "77f3e93fabcf300ba6c2d268e2e89baabed6cbf3";
          hash = "bgF3WhIl9/NHeTS3wPFtLOily1y8W952DVOPWes2Lkg=";
        }
        {
          name = "reversed_rx_character_range";
          rev = "446868de424170be0584980d4dcc0859f7077d54";
          hash = "HZtkpiTh+jD9pudYP+aAixPJx07R/BRXQ4Eg3OZ4gOA=";
        }
        {
          name = "ellipsize_nicks";
          rev = "6f655866206eac179f74637c77e6c4b259f9891c";
          hash = "wC/W09BL8219p/wPXRoIK4IlDfqC2aYq9/5ddnCf6K0=";
        }
        {
          name = "reverse_speedbar_order";
          rev = "2c572326c26d1f62bce2926b3cb00c7c2e971406";
          hash = "o21h6K0LKTPE1hNEShyqJw1YEg1ojOrOdQElEYRhzPY=";
        }
       #{
       #  name = "add_faces_for_irc_line_types";
       #  rev = "0dbb8720318d5041d13d68fb174aba2b3ab9b8b8";
       #  hash = "6zwMI+53M0RIykuG7SzvZ2XRSxsw65oAZSPIpki3JXA=";
       #}
      ];
    });
  };
in with prev.lib; rec {
  emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope' emacsOverride;

  emacsGcc = prev.emacsGcc.overrideAttrs (drv: {
    passthru = drv.passthru // { nativeComp = true; };
  });

  emacsPgtkGcc = (prev.emacsPgtkGcc.override {
    inherit (final) gsettings-desktop-schemas;
    withXwidgets = true;
    inherit (final) webkitgtk wrapGAppsHook glib-networking;
  }).overrideAttrs (drv: {
    gstBuildInputs = with final; with gst_all_1; [
      gstreamer gst-libav
      gst-plugins-base
      gst-plugins-good
      gst-plugins-bad
      gst-plugins-ugly
    ];
    buildInputs = drv.buildInputs ++ [
    ] ++ gstBuildInputs;

    GIO_EXTRA_MODULES = "${final.glib-networking}/lib/gio/modules:${final.dconf.lib}/lib/gio/modules";
    GST_PLUGIN_SYSTEM_PATH_1_0 = final.lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs;

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
