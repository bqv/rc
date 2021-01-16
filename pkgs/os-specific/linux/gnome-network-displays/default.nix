{ stdenv, flake, meson, ninja, pkg-config, wrapGAppsHook, writeText, python3
, gtk3, glib, gsettings-desktop-schemas, gst_all_1, networkmanager, libpulseaudio, desktop-file-utils }:

stdenv.mkDerivation rec {
  pname = "gnome-network-displays";
  version = src.shortRev;

  src = flake.inputs.gnome-network-displays;

  nativeBuildInputs = [ meson ninja pkg-config wrapGAppsHook ];
  buildInputs = [ gtk3 glib gsettings-desktop-schemas networkmanager libpulseaudio python3 desktop-file-utils ]
  ++ (with gst_all_1; [ gstreamer gst-plugins-base gst-plugins-good gst-plugins-bad gst-rtsp-server ]);

  NIX_CFLAGS_COMPILE = "-I${glib.dev}/include/gio-unix-2.0";
  postPatch = ''
    chmod +x build-aux/meson/postinstall.py
    patchShebangs build-aux/meson/postinstall.py
  '';
}
