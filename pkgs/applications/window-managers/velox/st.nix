{ stdenv, lib, fetchFromGitHub, pkgconfig, writeText
, ncurses, wayland, wayland-protocols, wld, libxkbcommon, fontconfig, pixman
, conf, patches }:

stdenv.mkDerivation rec {
  name = "st-velox-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "michaelforney";
    repo = "st";
    rev = "83d6bc6e0efccff6905374c1a5fb550f7c08bd4a";
    sha256 = "0hfrbyr5vck9f4appphk4wbg27l39vyxzlb8rjg4j1gk9pn44b1s";
    # date = 2019-10-14T10:50:23+01:00;
  };

  inherit patches;

  configFile = lib.optionalString (conf!=null) (writeText "config.def.h" conf);
  preBuild = lib.optionalString (conf!=null) "cp ${configFile} config.def.h";

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ ncurses wayland wayland-protocols wld libxkbcommon fontconfig pixman ];

  NIX_LDFLAGS = "-lfontconfig";

  installPhase = ''
    TERMINFO=$out/share/terminfo make install PREFIX=$out
  '';

  preFixup = ''
    mv $out/bin/st $out/bin/st-wl
  '';

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    homepage = https://st.suckless.org/;
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = with platforms; linux;
  };
}
