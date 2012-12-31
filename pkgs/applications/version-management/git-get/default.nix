{ lib, buildGoModule, withSources, makeWrapper }:

buildGoModule rec {
  pname = "git-get";
  version = src.shortRev;
  vendorSha256 = "05k6w4knk7fdjm9qm272nlrk47rzjr18g0fp4j57f5ncq26cxr8b";

  src = withSources.git-get;

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
