{ mkDerivation, base, bytestring, libinput, stdenv, text, fetchFromGitHub }:

mkDerivation {
  pname = "libinput";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "ivanmalison";
   #owner = "waymonad";
    repo = "libinput";
    rev = "2bcd7b10e0047612dbe9cef4d533eec72aaa734d";
    sha256 = "08rzm4skqnqm0yw5pl7w25ibbmw4yxdpq394i8z70gr6rnkcmjhj";
  };
  libraryHaskellDepends = [ base bytestring text ];
  libraryPkgconfigDepends = [ libinput ];
  license = stdenv.lib.licenses.lgpl21;
}
