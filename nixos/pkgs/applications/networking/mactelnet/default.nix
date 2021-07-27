{ lib, withSources, stdenv, autoreconfHook, ... }:

stdenv.mkDerivation rec {
  name = "mactelnet";
  src = withSources.mactelnet;
  version = src.shortRev;

  postPatch = "sed -i '/chown/d' config/Makefile.am";

  nativeBuildInputs = [ autoreconfHook ];

  buildPhase = "make all";
  installPhase = ''
    make install; mv $out/etc/mactelnetd.users{,.sample}; ln -s /etc/mactelnetd.users $out/etc/mactelnetd.users
  '';
}
