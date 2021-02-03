{ lib, stdenv, withSources, runtimeShell, getopt }:

stdenv.mkDerivation rec {
  pname = "shflags";
  version = src.shortRev;

  src = withSources.shflags;

  buildInputs = [ getopt ];

  doCheck = true;
  checkPhase = ''
    ./test_runner -s ${runtimeShell}
  '';

  installPhase = ''
    install -D -m 755 ./shflags $out
  '';

  meta = with lib; {
    description = "A port of the Google gflags library for Unix shell";
    homepage = https://github.com/kward/shflags;
    license = licenses.asl20;
    maintainers = [ maintainers.bqv ];
    platforms = platforms.unix;
  };
}
