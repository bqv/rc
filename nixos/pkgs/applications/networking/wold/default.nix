{ lib, withSources, stdenv, ... }:

stdenv.mkDerivation rec {
  name = "wold";
  src = withSources.wold;
  version = src.shortRev;

  buildPhase = "make all";
  installPhase = ''
    make install PREFIX=$out
  '';
}
