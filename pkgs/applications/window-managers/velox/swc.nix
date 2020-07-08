{ lib, stdenv, fetchFromGitHub, pkgconfig, makeWrapper
, wld, wayland, wayland-protocols, fontconfig, pixman, libdrm, libinput, libevdev, libxkbcommon, libxcb, xcbutilwm
}:

stdenv.mkDerivation rec {
  name = "swc-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "michaelforney";
    repo = "swc";
    rev = "1fe3b4d45f9e4f03f92401f6c771a2cd60047029";
    sha256 = "13p2p211hyvsf3w6bnciy1qf869gdvjqgppyvcfhb96w6g1jbanb";
    # date = 2020-07-07T16:15:49-07:00;
  };

  nativeBuildInputs = [ pkgconfig makeWrapper ];

  buildInputs = [ wld wayland wayland-protocols fontconfig pixman libdrm libinput libevdev libxkbcommon libxcb xcbutilwm ];

  prePatch = ''
    substituteInPlace launch/local.mk --replace 4755 755
  '';

  makeFlags = "PREFIX=$(out) ENABLE_XWAYLAND=1";
  installPhase = "PREFIX=$out make install";

  enableParallelBuilding = true;

  meta = {
    description = "A library for making a simple Wayland compositor";
    homepage    = src.meta.homepage;
    license     = lib.licenses.mit;
    platforms   = lib.platforms.linux;
    maintainers = with lib.maintainers; [ ];
  };
}
