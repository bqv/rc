inputs@{...}: final: prev: {
  cordless = with prev.cordless; final.buildGoModule {
    name = "cordless";
    src = pkgs.fetchFromGitHub {
      owner = "bios-marcel";
      repo = "cordless";
      rev = "2d5a3c39eb9d2fdd934c3cb5c3c7ebfc2f5c6761";
      sha256 = "1k78jna4aqvai94hg021khl9d4lmkgxgdqrr32k8vwfm48310f8i";
    };
    vendorSha256 = "XnwTqd19q+hOJZsfnFExiPDbg4pzV1Z9A6cq/jhcVgU=";
    inherit (pkgs.cordless) version meta;
  };
}
