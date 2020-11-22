{ lib, stdenv, flake, pkgconfig
, wayland, fontconfig, pixman, freetype, libdrm
}:

stdenv.mkDerivation rec {
  name = "wld-${version}";
  version = flake.inputs.wld.shortRev;

  src = flake.inputs.wld;

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ wayland fontconfig pixman freetype libdrm ];

  makeFlags = "PREFIX=$(out)";
  installPhase = "PREFIX=$out make install";

  enableParallelBuilding = true;

  meta = {
    description = "A primitive drawing library targeted at Wayland";
    homepage    = src.meta.homepage;
    license     = lib.licenses.mit;
    platforms   = lib.platforms.linux;
    maintainers = with lib.maintainers; [ ];
  };
}
