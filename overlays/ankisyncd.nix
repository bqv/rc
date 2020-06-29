final: prev: {
  ankisyncd = prev.ankisyncd.overrideAttrs (_: {
    src = final.fetchFromGitHub {
      owner = "ankicommunity";
      repo = "anki-sync-server";
      rev = "125f7bb1b58ccdeeaa710d4500844b7a4cfae85c";
      sha256 = "3LBdVegRTCYsQkJ87ZFjHW04SPZrqE3TGDGOM+PnNgk=";
    };
  });
}
