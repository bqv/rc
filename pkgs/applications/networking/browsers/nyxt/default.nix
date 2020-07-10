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

  nativeBuildInputs = [ git cacert makeWrapper wrapGAppsHook ];
  buildInputs = [
    sbcl openssl libfixposix
    glib gdk-pixbuf cairo
    pango gtk3 webkitgtk
    glib-networking gsettings-desktop-schemas
    xclip notify-osd enchant
  ];

  dontStrip = true;
  buildPhase = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    export HOME=$PWD && make build-deps nyxt
  '';

  dontWrapGApps = true;
  installPhase = ''
    make DESTDIR=$out PREFIX=/ install
    wrapProgram $out/bin/nyxt \
      --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
      "''${gappsWrapperArgs[@]}"
  '';

  __noChroot = true;
}
