{ lib, stdenv, withSources, pkgconfig
, wayland, fontconfig, pixman, freetype, libdrm
}:

stdenv.mkDerivation rec {
  name = "wld-${version}";
  version = src.shortRev;

  src = withSources.wld;

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
