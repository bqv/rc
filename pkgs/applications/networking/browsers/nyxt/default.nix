{ pkgs }: with pkgs;

stdenv.mkDerivation rec {
  pname = "nyxt";
  version = lib.substring 0 7 src.rev;

  src = fetchgit {
    url = "https://github.com/atlas-engineer/nyxt";
    rev = "8251d8b076e399d0e85ad75431f32ffdd452978f";
    sha256 = "lEY5qNhJayRmSjdCQuiS9COY7pVRHRwiq9iSCatdL78=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ git cacert ];
  buildInputs = [
    sbcl
    openssl
    libfixposix
    glib
    gdk-pixbuf
    cairo
    pango
    gtk3
    webkitgtk
  ];

  buildPhase = ''
    export HOME=$PWD
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    NYXT_INTERNAL_QUICKLISP=true make build-deps nyxt
  '';

  installPhase = ''
    make DESTDIR=$out PREFIX=/ install
    find $out
  '';
}
