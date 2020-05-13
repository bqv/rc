{ stdenv, lib, system, fetchFromGitHub, sbcl, curl, cacert }:

stdenv.mkDerivation rec {
  pname = "next";
  version = "2.0.0-git-${lib.substring 0 8 src.rev}";

  src = fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "next";
    rev = "ff499f8012215ac4c5a4ac8de63171125f222aba";
    sha256 = "1gcw7smmdxsy41vvxf3j8h8nflb9nwp7dniri8la4fiph18z4kj8";
  };

  nativeBuildInputs = [ sbcl curl cacert ];

  buildPhase = ''
    export HOME=/tmp
    make quicklisp/setup.lisp
    make all DESTDIR=$out NEXT_INTERNAL_QUICKLISP=true
  '';

 #installPhase = ''
 #  install -D -m0755 next $out/bin/next
 #'';

  # Stripping destroys the generated SBCL image
  dontStrip = true;

  outputHash = "0000000000000000000000000000000000000000000000000000";
  outputHashAlgo = "sha256";

  meta = with lib; {
    description = "Infinitely extensible web-browser (with Lisp development files using WebKitGTK platform port)";
    homepage = https://next.atlas.engineer;
    license = licenses.bsd3;
    platforms = [ "x86_64-linux" ];
    broken = system != "x86_64-linux";
  };
}
