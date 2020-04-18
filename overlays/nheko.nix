final: prev: let 
  lib = final.lib;
  boost = final.symlinkJoin {
    name = "boost-combined";
    paths = with final.boost171; [ out dev ];
  };
  tweeny = final.stdenv.mkDerivation rec {
    pname = "tweeny";
    version = lib.substring 0 9 src.rev;
    src = final.fetchFromGitHub {
      owner = "mobius3";
      repo = "tweeny";
      rev = "6a5033372fe53c4c731c66c8a2d56261746cd85c";
      hash = "sha256-GvRoheHGYVOczt0Cvq/Ov0eiB7Y8VEFhEV5FWJg0WT4=";
    };
    nativeBuildInputs = with final; [ cmake ];
  };
  mtxclient = prev.mtxclient.overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = final.fetchFromGitHub {
      owner = "nheko-reborn";
      repo = "mtxclient";
      rev = "61ddbb23893b1079e1b3d4bcf4aeca7718b7faa5";
      hash = "sha256-wmlXEd5IKZzkEonWzK7Kn7JNla0tPzf1piU6b2ztxW0=";
    };
  });
  nheko = prev.nheko.overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = final.fetchFromGitHub {
      owner = "nheko-reborn";
      repo = "nheko";
      rev = "076a1c3607556e2d6f21309e18945480c6637438";
      hash = "sha256-VVzpRn4zMuVeRamOtVtQ4AFR1nLGW3KYYnqR8kBYFe0=";
    };
    buildInputs = super.buildInputs ++ [
      final.qt5.qtquickcontrols2
      final.lmdbxx
      tweeny
    ];
  });
in {
  nheko = nheko.override {
    inherit boost;
    mtxclient = mtxclient.override {
      inherit boost;
    };
  };
}
