{ lib, stdenv, flake, pkgconfig, makeWrapper
, wld, wayland, wayland-protocols, fontconfig, pixman, libdrm, libinput, libevdev, libxkbcommon, libxcb, xcbutilwm
}:

stdenv.mkDerivation rec {
  name = "swc-${version}";
  version = flake.inputs.swc.shortRev;

  src = flake.inputs.swc;

  patches = [
   #./hardcode-screen-order.patch # committed
  ];

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
