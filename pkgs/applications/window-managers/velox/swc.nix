{ lib, stdenv, fetchFromGitHub, pkgconfig
, wld, wayland, wayland-protocols, fontconfig, pixman, libdrm, libinput, libevdev, libxkbcommon, libxcb, xcbutilwm
}:

stdenv.mkDerivation rec {
  name = "swc-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "michaelforney";
    repo = "swc";
    rev = "86b45d5701e509660650facdad4f7bef8f4f5362";
    sha256 = "0hyy6fyih99fs09wlywkgr4kfj87addncijzsywm1v692yrv7i7r";
    # date = 2020-02-28T13:27:59-08:00;
  };

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ wld wayland wayland-protocols fontconfig pixman libdrm libinput libevdev libxkbcommon libxcb xcbutilwm ];

  prePatch = ''
    substituteInPlace launch/local.mk --replace 4755 755
  '';

  makeFlags = "PREFIX=$(out)";
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
