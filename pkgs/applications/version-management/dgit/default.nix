{ stdenv, buildGoModule, fetchFromGitHub, makeWrapper,
  loglevel ? "" }:

buildGoModule rec {
  pname = "dgit";
  version = "0.0.14-alpha";
  modSha256 = "02mam8a26qp2i0i8cni5id8nw6qa18yxywic6p4d0bxg3aycrg89";

  src = fetchFromGitHub {
    owner = "quorumcontrol";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-8l3iWJmu/j2Ielr1rr1Lu4/rzzDhWMgJPxkThizjncI=";
  };

  nativeBuildInputs = [ makeWrapper ];
  postInstall = ''
    mkdir -p $out/bin
    makeWrapper $out/bin/dgit $out/bin/git-remote-dgit \
      --add-flags remote-helper --set DGIT_LOG_LEVEL $loglevel
  '';

  meta = with stdenv.lib; {
    description = "Decentralized git ownership and storage.";
    homepage = https://dgit.dev/;
    license = licenses.mit;
    platforms = platforms.unix;
  };
}

