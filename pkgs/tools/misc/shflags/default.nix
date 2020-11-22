{ stdenv, flake, runtimeShell, getopt }:

stdenv.mkDerivation rec {
  pname = "shflags";
  version = flake.inputs.shflags.shortRev;

  src = flake.inputs.shflags;

  buildInputs = [ getopt ];

  doCheck = true;
  checkPhase = ''
    ./test_runner -s ${runtimeShell}
  '';

  installPhase = ''
    install -D -m 755 ./shflags $out
  '';

  meta = with stdenv.lib; {
    description = "A port of the Google gflags library for Unix shell";
    homepage = https://github.com/kward/shflags;
    license = licenses.asl20;
    maintainers = [ maintainers.bqv ];
    platforms = platforms.unix;
  };
}
