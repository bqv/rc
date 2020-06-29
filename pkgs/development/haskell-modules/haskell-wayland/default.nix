{ mkDerivation, base, c2hs, data-flags, libGL, pkg-config, process
, stdenv, template-haskell, time, transformers, wayland, xml
, fetchFromGitHub }:

mkDerivation {
  pname = "hayland";
  version = "0.1.1.0";
  src = fetchFromGitHub {
    owner = "ongy";
    repo = "haskell-wayland";
    rev = "4a8974dcb0adf50b14e822633f7fc84aaa4afa70";
    sha256 = "13yjdlcixz6lxdkfavsdc5mg3lc9bphklj94cyznrbbiavjzrr7s";
  };
  doCheck = false; # Attempts to use a wayland server?
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base data-flags process template-haskell time transformers xml
  ];
  libraryPkgconfigDepends = [ libGL wayland ];
  libraryToolDepends = [ c2hs pkg-config ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base process xml ];
  description = "Haskell bindings for the C Wayland library";
  license = stdenv.lib.licenses.mit;
}
