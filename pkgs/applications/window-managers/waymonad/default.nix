{ mkDerivation, base, bytestring, clock, composition, config-schema
, config-value, containers, data-default, deepseq, directory
, formatting, ghc-prim, hayland, HFuse, hsroots, libinput
, libxkbcommon, mtl, network, process, safe, semigroupoids, stdenv
, stm, template-haskell, text, time, transformers, unix, unliftio
, unliftio-core, waymonad-scanner, xdg-basedir, xkbcommon
, fetchFromGitHub, pkg-config }:

mkDerivation {
  pname = "waymonad";
  version = "0.0.1.0";
  src = ./waymonad;
 #src = fetchFromGitHub {
 #  owner = "bqv";
 # #owner = "waymonad";
 #  repo = "waymonad";
 #  rev = "910db0362b173e19c0a9a69d1c32cde62cb9614a";
 #  sha256 = "0ay8f39jp3384sjvbr4c3psdkmj5agv5bf1vy5vifk580sd7x1l7";
 #};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring clock composition config-schema config-value
    containers data-default deepseq directory formatting ghc-prim
    hayland HFuse hsroots libinput mtl network process safe
    semigroupoids stm template-haskell text time transformers unix
    unliftio unliftio-core waymonad-scanner xdg-basedir xkbcommon
  ];
  librarySystemDepends = [ libxkbcommon ];
  libraryToolDepends = [ pkg-config ];
  executableHaskellDepends = [
    base containers hayland hsroots libinput text xkbcommon
  ];
  homepage = "https://github.com/ongy/waymonad";
  description = "Wayland compositor build on the ideas of Xmonad";
  license = stdenv.lib.licenses.lgpl21;
}
