{ mkDerivation, base, bytestring, cereal, containers, hayland
, hsroots, mtl, process, stdenv, template-haskell, text, wayland
, xml, fetchFromGitHub }:

mkDerivation {
  pname = "waymonad-scanner";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "bqv";
   #owner = "ongy";
    repo = "waymonad-scanner";
    rev = "10679a19ab01fbec30dbcffd04e6a5c747012d42";
    sha256 = "1q7hf166dlgyhhmfn588wpfjsm514j2m2bixdb6vm5m7gpx8dx1c";
  };
  libraryHaskellDepends = [
    base bytestring cereal containers hayland hsroots mtl process
    template-haskell text xml
  ];
  libraryPkgconfigDepends = [ wayland ];
  license = stdenv.lib.licenses.lgpl21;
}
