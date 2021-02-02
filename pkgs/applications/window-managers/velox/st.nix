{ stdenv, lib, withSources, pkgconfig, writeText
, ncurses, wayland, wayland-protocols, wld, libxkbcommon, fontconfig, pixman
, conf, patches }:

stdenv.mkDerivation rec {
  name = "st-velox-${version}";
  version = src.shortRev;

  src = withSources.st-wl;

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

  meta = with lib; {
    homepage = https://st.suckless.org/;
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = with platforms; linux;
  };
}
