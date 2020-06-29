{ mkDerivation, base, bytestring, fuse, stdenv, unix, fetchFromGitHub }:

mkDerivation {
  pname = "HFuse";
  version = "0.2.4.4";
  src = fetchFromGitHub {
    owner = "ongy";
    repo = "hfuse";
    rev = "33b156581136ea2f471ab40721cdb1862544d7b1";
    sha256 = "0xw180hdzzfnwn062blfa6a5083sm0nrksl5aia1wk8b6770qli7";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring unix ];
  librarySystemDepends = [ fuse ];
  preConfigure = ''
    sed -i -e "s@  Extra-Lib-Dirs:         /usr/local/lib@  Extra-Lib-Dirs:         ${fuse}/lib@" HFuse.cabal
  '';
  homepage = "https://github.com/m15k/hfuse";
  description = "HFuse is a binding for the Linux FUSE library";
  license = stdenv.lib.licenses.bsd3;
}
