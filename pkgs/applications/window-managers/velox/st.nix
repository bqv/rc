{ stdenv, lib, fetchFromGitHub, pkgconfig, writeText
, ncurses, wayland, wayland-protocols, wld, libxkbcommon, fontconfig, pixman, xorg
, conf, patches }:

stdenv.mkDerivation rec {
  name = "st-velox-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "michaelforney";
    repo = "st";
    rev = "c63a87cd936c1eeef14c4c21572e5b782d3df4bc";
    sha256 = "1fnzpka964wvavzcgis27k9px2bvgav7dccxfpd73dh1xiflqald";
    # date = 2016-12-16T10:50:23+01:00;
  };

  inherit patches;

  configFile = lib.optionalString (conf!=null) (writeText "config.def.h" conf);
  preBuild = lib.optionalString (conf!=null) "cp ${configFile} config.def.h";

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ ncurses wayland wayland-protocols wld libxkbcommon fontconfig pixman ]
    ++ (with xorg; [ libX11 libXft ]);

  NIX_LDFLAGS = "-lfontconfig";

  installPhase = ''
    TERMINFO=$out/share/terminfo make install PREFIX=$out
  '';

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    homepage = https://st.suckless.org/;
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = with platforms; linux;
  };
}
