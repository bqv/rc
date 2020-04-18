{ naersk, fetchFromGitHub, lib, ... }:

naersk.buildPackage {
  pname = "weechat-matrix";
  version = "0.1.0-20200418-git";

  src = fetchFromGitHub {
    owner = "poljar";
    repo = "weechat-matrix-rs";
    rev = "74742a31a8d0e2678ec18a16df73430340f63b2a";
    hash = "sha256-S08s2A+l6G685VFwTeU0QmL8Cr06c/JOPzlJYRdytAU=";
  };

  meta = with lib; {
    description = "Matrix! In Weechat!";
    homepage = src.url;
    license = licenses.isc;
    maintainers = with maintainers; [ bqv ];
    platforms = [ "x86_64-linux" ];
  };
}
