{ lib, stdenv, fetchFromGitHub, pkgconfig, makeWrapper
, wld, wayland, wayland-protocols, fontconfig, pixman, libdrm, libinput, libevdev, libxkbcommon, libxcb, xcbutilwm
}:

stdenv.mkDerivation rec {
  name = "swc-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "bqv";
    repo = "swc";
    rev = "2d5a062c22f944f888f739ae289a6878babf7a81";
    sha256 = "0znbnyaym718dfzb78ahcajd6ncfr89mjv7dpb652phri5qprcwg";
    # date = 2020-07-13T01:30:15+01:00;
  };

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
