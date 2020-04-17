final: prev: let 
  lib = final.lib;
  matrix-nio = prev.python3Packages.matrix-nio.overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = final.fetchFromGitHub {
      owner = "poljar";
      repo = "matrix-nio";
      rev = "98f0c244065ea59c2e5105bc0aab5811aea748cf";
      hash = "sha256-GQPTnGxazR3WW8WGrC4X1oXvDXPMqQ5AZxdJns87C/Q=";
    };
    patches = [
      (final.fetchpatch {
        url = "https://github.com/bqv/matrix-nio/commit/dfb2f9d12d557aff5bb07f01ac9691d8908bbf67.patch";
        sha256 = "0x378cq71y15ri6g3fznfcg8ws4nrcfpaxcv38dzmlbizg08gwzg";
      })
    ];
  });
  weechat-matrix = prev.weechatScripts.weechat-matrix.overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = final.fetchFromGitHub {
      owner = "poljar";
      repo = "weechat-matrix";
      rev = "d415841662549f096dda09390bfdebd3ca597bac";
      hash = "sha256-QT3JNzIShaR8vlrWuGzBtLDHjn7Z6vhovcOAfttgUxo=";
    };
  });
in {
  weechatScripts = prev.weechatScripts // {
    weechat-matrix = weechat-matrix.override {
      inherit matrix-nio;
    };
  };
}
