{ lib, buildGoModule, fetchFromGitHub, makeWrapper }:

buildGoModule rec {
  pname = "git-get";
  version = lib.substring 0 8 src.rev;
  vendorSha256 = "05k6w4knk7fdjm9qm272nlrk47rzjr18g0fp4j57f5ncq26cxr8b";

  src = fetchFromGitHub {
    owner = "grdl";
    repo = "git-get";
    rev = "f610fd9930bc484223b225bb79c88efaaac33c85";
    sha256 = "05g34mk1n3iics4wn8bzmx4xz4hlaxylwwg3pln4csiw9zwrxzi0";
    # date = 2020-07-31T14:23:22+02:00;
  };

  nativeBuildInputs = [ makeWrapper ];
  postInstall = ''
    mkdir -p $out/bin
    wrapProgram $out/bin/get
    wrapProgram $out/bin/list
    mv $out/bin/get  $out/bin/git-get 
    mv $out/bin/list $out/bin/git-list
  '';

  doCheck = false;

  meta = with lib; {
    platforms = platforms.unix;
  };
}
