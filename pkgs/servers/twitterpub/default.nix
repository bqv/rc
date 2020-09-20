{ buildGoModule, fetchFromGitHub, lib }:

buildGoModule rec {
  pname = "twitterpub";
  version = lib.substring 0 7 src.rev;

  vendorSha256 = "XuQhbS9iLNrW6aSanPdJFJO07JbJ7/QPUlGiYo0WZoE=";

  src = fetchFromGitHub {
    owner = "bqv";
    repo = "twitterpub";
    rev = "5ef96de25a0d77f815564e35bccd330403dc32e7";
    sha256 = "ypMfbs+61oxAbpN0vCzMCxJ3ZgNHhq+Km9m656HuYdk=";
  };
}
