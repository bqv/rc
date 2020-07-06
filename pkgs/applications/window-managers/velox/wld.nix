{ lib, stdenv, fetchFromGitHub, pkgconfig
, wayland, fontconfig, pixman, freetype, libdrm
}:

stdenv.mkDerivation rec {
  name = "wld-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "michaelforney";
    repo = "wld";
    rev = "ea4eccb64cfcfc508b029a530fc434d6e6695af5";
    sha256 = "0ynvqd3cwy23q97rn2x8v5gldpc3aa37nx66p8cd4cgpvcwvps5z";
    # date = 2020-02-20T13:50:32-08:00;
  };

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
