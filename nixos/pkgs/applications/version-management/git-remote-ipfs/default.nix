{ lib, buildGoModule, withSources, makeWrapper, git }:

buildGoModule rec {
  pname = "git-remote-ipfs";
  version = src.shortRev;
  vendorSha256 = "hkenInaS6PFnu/Z0oz32Y4B4BmM5+l5AB2/K1f/LxqA=";

  src = withSources.git-remote-ipfs;

  doCheck = false;
  checkInputs = [
    git
  ];

  postInstall = ''
    ln -s $out/bin/git-remote-ipfs $out/bin/git-remote-ipns
  '';

  meta = with lib; {
    platforms = platforms.unix;
  };
}
