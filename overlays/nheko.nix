final: prev: let 
  lib = final.lib;
  mtxclient = prev.mtxclient.overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = final.fetchFromGitHub {
      owner = "nheko-reborn";
      repo = "mtxclient";
      rev = "61ddbb23893b1079e1b3d4bcf4aeca7718b7faa5";
      hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    };
  });
  nheko = prev.nheko.overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = final.fetchFromGitHub {
      owner = "nheko-reborn";
      repo = "nheko";
      rev = "076a1c3607556e2d6f21309e18945480c6637438";
      hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    };
  });
in {
  nheko = nheko.override {
    inherit mtxclient;
  };
}
