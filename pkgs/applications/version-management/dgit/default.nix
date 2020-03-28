{ stdenv, buildGoModule, fetchFromGitHub, makeWrapper,
  loglevel ? "" }:

buildGoModule rec {
  pname = "dgit";
  version = "0.0.11-alpha";
  modSha256 = "1jafi3sk28pbqgrxcr20gfywrgsq3rv0srl8fa2whxqw945finwf";

  src = fetchFromGitHub {
    owner = "quorumcontrol";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-g3zsIL8t1BVZdXHfbCCmQGDTfOnDagKG8mkLZh9i7/g=";
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
