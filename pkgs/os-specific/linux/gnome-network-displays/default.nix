{ stdenv, fetchFromGitLab, meson, ninja, pkg-config, wrapGAppsHook, writeText, python3
, gtk3, glib, gsettings-desktop-schemas, gst_all_1, networkmanager, libpulseaudio, desktop-file-utils }:

stdenv.mkDerivation rec {
  pname = "gnome-network-displays";
  version = "0.90.3";

  src = fetchFromGitLab {
    domain = "gitlab.gnome.org";
    owner = "GNOME";
    repo = "gnome-network-displays";
    rev = "7a4bf6caeb6538a5f0a322695645cc45cda8fc1f";
    sha256 = "1fwyd817xacnm5hvphyadi3xz0436mpc5j1jglh61kxs8mk0a51d";
  };

  nativeBuildInputs = [ meson ninja pkg-config wrapGAppsHook ];
  buildInputs = [ gtk3 glib gsettings-desktop-schemas networkmanager libpulseaudio python3 desktop-file-utils ]
  ++ (with gst_all_1; [ gstreamer gst-plugins-base gst-plugins-good gst-plugins-bad ])
  ++ [ (gst_all_1.gst-rtsp-server.overrideAttrs (super: {
    patches = let
      prefix = "\${prefix}";
    in (super.patches or []) ++ [
      (writeText "fix_pkgconfig_includedir.patch" ''
        diff --git a/pkgconfig/meson.build b/pkgconfig/meson.build
        index edb0586c2..7ed46dfce 100644
        --- a/pkgconfig/meson.build
        +++ b/pkgconfig/meson.build
        @@ -2,8 +2,8 @@ pkgconf = configuration_data()

         pkgconf.set('prefix', get_option('prefix'))
         pkgconf.set('exec_prefix', '${prefix}')
        -pkgconf.set('libdir', '${prefix}/@0@'.format(get_option('libdir')))
        -pkgconf.set('includedir', '${prefix}/@0@'.format(get_option('includedir')))
        +pkgconf.set('libdir', join_paths(get_option('prefix'), get_option('libdir')))
        +pkgconf.set('includedir', join_paths(get_option('prefix'), get_option('includedir')))
         pkgconf.set('GST_API_VERSION', api_version)
         pkgconf.set('VERSION', gst_version)

      '')
    ];
  })) ];

  NIX_CFLAGS_COMPILE = "-I${glib.dev}/include/gio-unix-2.0";
  postPatch = ''
    chmod +x build-aux/meson/postinstall.py
    patchShebangs build-aux/meson/postinstall.py
  '';
}
