{ stdenv, fetchurl, boringssl }:

stdenv.mkDerivation rec {
  pname         = "arm-adb";
  name          = "${pname}-${version}";
  version       = "1.0.39";
  revisiondate  = "20191121";
  revisionbuild = "2db4642";

  src = fetchurl {
    url = "https://github.com/qhuyduong/arm_${pname}/archive/v${version}.tar.gz";
    sha256 = "1v591722cbd4caz2bhf9b5hnrdzb1br7bghb19vl4kdpc9hj98rv";
  };

  dontDisableStatic = true;

  configurePhase = "./configure --prefix=$out/ --includedir=${boringssl}/include/ --libdir=${boringssl}/lib/";

  installPhase = ''
    mkdir -p $out/lib
    make install libdir=$out/lib/
  '';

  buildInputs = [ boringssl ];

  meta = with stdenv.lib; {
    description = "Android Debug Bridge";
    homepage    = "https://github.com/qhuyduong/arm_adb";
    license     = licenses.gpl3;
    platforms   = platforms.linux;
    maintainers = with maintainers; [ ];
  };
}
