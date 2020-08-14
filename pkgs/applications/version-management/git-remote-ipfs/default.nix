{ lib, buildGoModule, fetchFromGitHub, makeWrapper, git }:

buildGoModule rec {
  pname = "git-remote-ipfs";
  version = lib.substring 0 8 src.rev;
  vendorSha256 = "14m0kalmjrf49bqp5lnkrhp3crmqfhnjh10iyxhn50hji1cb6lzd";

  src = fetchFromGitHub {
    owner = "cryptix";
    repo = "git-remote-ipfs";
    rev = "241d12485ebb3fccf2db32e6e24ba9f6035d7435";
    sha256 = "0vckpkrcvcinrw92w67nx0fkfga3pj7g2z26hghhdr1kv0lgch21";
    # date = 2019-12-19T11:11:24+01:00;
  };

  doCheck = false;
  checkInputs = [
    git
  ];

  meta = with lib; {
    platforms = platforms.unix;
  };
}
