{ lib, stdenv, withSources, pkgconfig, makeWrapper, newScope
, libxkbcommon, wayland, pixman, fontconfig, libinput
, stConf ? null, stPatches ? []
}:

let
  callPackage = newScope scope;
  scope = {
    swc = callPackage ./swc.nix {};
    wld = callPackage ./wld.nix {};
    dmenu-velox = callPackage ./dmenu.nix {};
    st-velox = callPackage ./st.nix {
      conf = stConf;
      patches = stPatches;
    };
  };
in stdenv.mkDerivation rec {
  name = "velox-${version}";
  version = src.shortRev;

  src = withSources.velox;

  nativeBuildInputs = [ pkgconfig makeWrapper ];

  buildInputs = [ libxkbcommon wayland pixman fontconfig libinput ];
  propagatedBuildInputs = with scope; [ swc wld ];

  propagatedUserEnvPkgs = with scope; [ swc ];

  makeFlags = "PREFIX=$(out)";
  preBuild = ''
    substituteInPlace config.c \
      --replace /etc/velox.conf $out/etc/velox.conf
  '';
  installPhase = ''
    PREFIX=$out make install
    mkdir -p $out/etc
    cp velox.conf.sample $out/etc/velox.conf
  '';
  postFixup = with scope; ''
    wrapProgram $out/bin/velox \
      --prefix PATH : "${stdenv.lib.makeBinPath [ dmenu-velox st-velox ]}"
  '';

  enableParallelBuilding = false; # https://hydra.nixos.org/build/79799608

  passthru = scope;

  meta = {
    description = "velox window manager";
    homepage    = "https://github.com/michaelforney/velox";
    license     = lib.licenses.mit;
    platforms   = lib.platforms.linux;
    maintainers = with lib.maintainers; [ ];
  };
}
