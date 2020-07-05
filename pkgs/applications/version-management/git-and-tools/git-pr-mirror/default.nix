{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "git-pull-request-mirror-unstable";
  version = "2020.06.04-git";

  vendorSha256 = "0789v1r6my256pncs0105yji28ifchj6ppfiy8gavglgclq3cgvn";

  src = fetchFromGitHub {
    owner = "google";
    repo = "git-pull-request-mirror";
    rev = "3d342363b0e1f2da4e59ac01baf4601bcf233bf9";
    sha256 = "13p1kjw5nvlv2d71094dslfd003a57qj3v5xvij19ms1ci453jrq";
  };

  postInstall = ''
    for bin in $out/bin/*; do
      mv $bin $out/bin/git-pr-$(basename $bin)
    done
  '';

  meta = with lib; {
    description = "Mirror Github Pull Requests into the git-pull-request-mirror formats";
    homepage = "https://github.com/google/git-pull-request-mirror";
    license = licenses.asl20;
    platforms = platforms.all;
    maintainers = [ ];
  };
}
