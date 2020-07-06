{stdenv, lib, fetchFromGitHub
, swc, wld, wayland, libxkbcommon, pixman, fontconfig, xorg
}:

stdenv.mkDerivation rec {
  name = "dmenu-velox-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "michaelforney";
    repo = "dmenu";
    rev = "5cd66e2c6ca6a82e59927d495498fa6e478594d6";
    sha256 = "1rzl0spv3ab5xbshza7jr47qbav0dakhf9ifpcs5lixahn9lfnkl";
    # date = 2016-12-11T12:33:16+01:00;
  };

  buildInputs = [ swc wld wayland libxkbcommon pixman fontconfig ]
    ++ (with xorg; [ libX11 libXft libXinerama ]);

  postPatch = ''
    sed -ri -e 's!\<(dmenu|dmenu_path)\>!'"$out/bin"'/&!g' dmenu_run
  '';

  preConfigure = [
    ''sed -i "s@PREFIX = /usr/local@PREFIX = $out@g; s@/usr/share/swc@${swc}/share/swc@g" config.mk''
  ];

  enableParallelBuilding = true;

  meta = {
    description = "A generic, highly customizable, and efficient menu for the X Window System";
    homepage = https://tools.suckless.org/dmenu;
    license = stdenv.lib.licenses.mit;
    maintainers = with stdenv.lib.maintainers; [ ];
    platforms = with stdenv.lib.platforms; all;
  };
}
