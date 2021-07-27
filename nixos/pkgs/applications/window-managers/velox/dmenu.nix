{ stdenv, lib, withSources, swc, wld, wayland, libxkbcommon, pixman, fontconfig }:

stdenv.mkDerivation rec {
  name = "dmenu-velox-${version}";
  version = src.shortRev;

  src = withSources.dmenu;

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
      sed -i 's!'"$out/bin/"'dmenu\b!&-wl!g' $out/bin/$i
      sed -i 's!'"$out/bin/"'dmenu_path\b!&-wl!g' $out/bin/$i
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
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ ];
    platforms = with lib.platforms; all;
  };
}
