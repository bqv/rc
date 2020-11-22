{ lib, buildGoModule, flake, makeWrapper, git }:

buildGoModule rec {
  pname = "git-remote-ipfs";
  version = flake.inputs.git-remote-ipfs.shortRev;
  vendorSha256 = "14m0kalmjrf49bqp5lnkrhp3crmqfhnjh10iyxhn50hji1cb6lzd";

  src = flake.inputs.git-remote-ipfs;

  doCheck = false;
  checkInputs = [
    git
  ];

  meta = with lib; {
    platforms = platforms.unix;
  };
}
