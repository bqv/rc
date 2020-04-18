final: prev: let 
  lib = final.lib;
  boost = final.symlinkJoin {
    name = "boost-combined";
    paths = with final.boost170; [ out dev ];
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
    buildInputs = super.buildInputs ++ [ final.qt5.qtquickcontrols2 ];
  });
in {
  nheko = nheko.override {
    mtxclient = mtxclient.override { inherit boost; };
  };
}
