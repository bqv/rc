inputs@{ matrix-nio, weechat-matrix, ... }: final: prev: let
  lib = final.lib;
  matrix-nio = prev.python3Packages.matrix-nio.overrideAttrs (super: rec {
    version = src.shortRev;
    src = inputs.matrix-nio;
    patches = [
      (final.fetchpatch {
        url = "https://github.com/bqv/matrix-nio/commit/fd66d5d5e2c33120b27c2af44022b18521ee8b43.patch";
        sha256 = "170q7apgxhj9wzaqw23ax7apc8sjrnq27689zi5yvwyg3ic3x99w";
      })
    ];
  });
  weechat-matrix = prev.weechatScripts.weechat-matrix.overrideAttrs (super: rec {
    version = src.shortRev;
    src = inputs.weechat-matrix;
  });
in {
  weechatScripts = prev.weechatScripts // {
    weechat-matrix = weechat-matrix.override {
      inherit matrix-nio;
    };
  };
}
