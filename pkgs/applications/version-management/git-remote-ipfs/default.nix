{ lib, buildGoModule, withSources, makeWrapper, git }:

buildGoModule rec {
  pname = "git-remote-ipfs";
  version = src.shortRev;
  vendorSha256 = "14m0kalmjrf49bqp5lnkrhp3crmqfhnjh10iyxhn50hji1cb6lzd";

  src = withSources.git-remote-ipfs;

  doCheck = false;
  checkInputs = [
    git
  ];

  meta = with lib; {
    platforms = platforms.unix;
  };
}
