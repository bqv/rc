{ stdenv, lib, fetchgit, pkgs }:

stdenv.mkDerivation {
  name = "taiwins";
  src = fetchgit {
    url = "https://github.com/taiwins/taiwins";
    rev = "722503b9eed47a8cc2dd53dd0b5b03d2f0a50c3f";
    sha256 = "l1DKuH8Q2Mp3NAFqe7weHPeXwh1Ux9eJ+f4gLE0qmNA=";
    fetchSubmodules = true;
  };
  nativeBuildInputs = with pkgs; [
    meson pkgconfig libxkbcommon wayland cairo cmake udev librsvg
    wayland-protocols lua5_3 dbus libdrm libinput mesa pam ninja pixman
    xorg.xcbutil xorg.libxcb xorg.xcbutilwm xorg.xcbutilerrors
  ];
  CFLAGS = [
    "-Wno-error=unused-result"
    "-Wno-error=maybe-uninitialized"
    "-Wno-error=stringop-overflow="
  ];
}
