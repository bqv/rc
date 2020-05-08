{ stdenv, fetchFromGitHub, runtimeShell, getopt }:

stdenv.mkDerivation rec {
  pname = "shflags";
  version = "1.2.3";

  src = fetchFromGitHub {
    owner = "kward";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-ntIso51KI6Z+XblLR+vGCf7rjiemrXJfEzr9bZYGvfk=";
  };

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
