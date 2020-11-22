{ lib, buildGoModule, flake }:

buildGoModule rec {
  pname = "git-pull-request-mirror-unstable";
  version = flake.inputs.git-pullrequest.shortRev;

  vendorSha256 = "0789v1r6my256pncs0105yji28ifchj6ppfiy8gavglgclq3cgvn";

  src = flake.inputs.git-pullrequest;

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
