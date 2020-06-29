{ mkDerivation, base, bytestring, composition, hayland, input
, libinput, libX11, pixman, stdenv, text, unix, wayland, wlroots
, xkbcommon, fetchFromGitHub, freerdp }:

let wlroots-patched = wlroots.overrideAttrs (o: rec {
  version = "0.6.0";
  src = fetchFromGitHub {
    owner = "swaywm";
    repo = "wlroots";
    rev = version;
    sha256 = "1rdcmll5b8w242n6yfjpsaprq280ck2jmbz46dxndhignxgda7k4";
  };
  buildInputs = o.buildInputs ++ [ freerdp ];

  mesonFlags = [
    "-Dlibcap=enabled" "-Dlogind=enabled" "-Dxwayland=enabled" "-Dx11-backend=enabled"
    "-Dxcb-icccm=enabled" "-Dxcb-errors=enabled"
  ];

  postPatch = ''
    # It happens from time to time that the version wasn't updated:
    sed -iE "s/version: '[0-9]\.[0-9]\.[0-9]'/version: '${version}.0'/" meson.build
  '';

  postInstall = ''
    # Copy the library to $bin and $examples
    for output in "$bin" "$examples"; do
      mkdir -p $output/lib
      cp -P libwlroots* $output/lib/
    done
  '';

  postFixup = ''
    # Install rootston (the reference compositor) to $bin and $examples (this
    # has to be done after the fixup phase to prevent broken binaries):
    for output in "$bin" "$examples"; do
      mkdir -p $output/bin
      cp rootston/rootston $output/bin/
      patchelf \
        --set-rpath "$(patchelf --print-rpath $output/bin/rootston | sed s,$out,$output,g)" \
        $output/bin/rootston
      mkdir $output/etc
      cp ../rootston/rootston.ini.example $output/etc/rootston.ini
    done
    # Install ALL example programs to $examples:
    # screencopy dmabuf-capture input-inhibitor layer-shell idle-inhibit idle
    # screenshot output-layout multi-pointer rotation tablet touch pointer
    # simple
    mkdir -p $examples/bin
    cd ./examples
    for binary in $(find . -executable -type f -printf '%P\n' | grep -vE '\.so'); do
      cp "$binary" "$examples/bin/wlroots-$binary"
      patchelf \
        --set-rpath "$(patchelf --print-rpath $output/bin/rootston | sed s,$out,$examples,g)" \
        "$examples/bin/wlroots-$binary"
    done
  '';
});
#in let wlroots = wlroots-patched;
in mkDerivation {
  pname = "hsroots";
  version = "0.1.0.0";
  src = let real = fetchFromGitHub {
    owner = "bqv";
    repo = "hsroots";
    rev = "a0c5a2608e2f68d52bc0f41a6a346da24ffc5295";
    sha256 = "1mnz093m0cbaqqjn45z1j80v1ll4j1lr3wi4c34yr2b61l7nh9lb";
  }; in ./hsroots;
  libraryHaskellDepends = [
    base bytestring composition hayland input text unix xkbcommon
  ];
  librarySystemDepends = [ input libX11 wayland ];
  libraryPkgconfigDepends = [ pixman wlroots ];
  description = "A small simple wrapper around wolroots";
  license = stdenv.lib.licenses.lgpl21;
}
