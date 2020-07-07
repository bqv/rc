{ stdenv, lib, fetchFromGitHub, swc, wld, wayland, libxkbcommon, pixman, fontconfig }:

stdenv.mkDerivation rec {
  name = "dmenu-velox-${version}";
  version = lib.substring 0 7 src.rev;

  src = fetchFromGitHub {
    owner = "michaelforney";
    repo = "dmenu";
    rev = "f385d9d18813071b4b4257bf8d4d572daeda0e70";
    sha256 = "14j8jv0nlybinhzkgd6dplvng9zy8p292prlx39w0k4fm6x5nv6y";
    # date = 2017-04-07T12:33:16+01:00;
  };

  buildInputs = [ swc wld wayland libxkbcommon pixman fontconfig ];

  postPatch = ''
    sed -ri -e 's!\<(dmenu|dmenu_path)\>!'"$out/bin"'/&!g' dmenu_run
  '';

  preConfigure = [
    ''sed -i "s@PREFIX = /usr/local@PREFIX = $out@g; s@/usr/share/swc@${swc}/share/swc@g" config.mk''
  ];

  preFixup = ''
    # Patch dmenu scripts to use binaries with -wl suffix.
    for i in dmenu_path dmenu_run; do
      sed -ri -e 's!\<(dmenu|stest)\>!'"$out/bin"'/&-wl!g' $i
    done
    # Rename all executables with the -wl suffix.
    for i in dmenu dmenu_path dmenu_run stest; do
      mv $out/bin/$i $out/bin/$i-wl
    done
  '';

  enableParallelBuilding = true;

  meta = {
    description = "A generic, highly customizable, and efficient menu for the X Window System";
    homepage = https://tools.suckless.org/dmenu;
    license = stdenv.lib.licenses.mit;
    maintainers = with stdenv.lib.maintainers; [ ];
    platforms = with stdenv.lib.platforms; all;
  };
}
