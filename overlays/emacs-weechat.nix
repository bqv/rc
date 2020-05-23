final: prev: let
  override = self: super: {
    weechat-patched = super.weechat.overrideAttrs (o: {
      patches = (o.patches or []) ++ [
        (final.fetchpatch {
          name = "rx-form_to_rx--translate-form.patch";
          url = "https://github.com/emacsomancer/weechat.el/commit/77f3e93fabcf300ba6c2d268e2e89baabed6cbf3.patch";
          sha256 = "0j1f6vmmk3sk1mvdwnxwbk5sbs1cdpqw1drlg53z7xr529d7f0bf";
        })
        (final.fetchpatch {
          name = "reversed_rx_character_range.patch";
          url = "https://github.com/eklitzke/weechat.el/commit/446868de424170be0584980d4dcc0859f7077d54.patch";
          sha256 = "1q40g3kdq8418dbi9z6i9v3wj4wbh3k3yn77lvyk1yp14jk696qx";
        })
        (final.fetchpatch {
          name = "ellipsize_nicks.patch";
          url = "https://github.com/bqv/weechat.el/commit/6f655866206eac179f74637c77e6c4b259f9891c.patch";
          sha256 = "1bg8kxq7cpgyywmadnc2z86jb0ib10d5s3zwlxynvwsbs39xcby0";
        })
        (final.fetchpatch {
          name = "reverse_speedbar_order.patch";
          url = "https://github.com/bqv/weechat.el/commit/2c572326c26d1f62bce2926b3cb00c7c2e971406.patch";
          sha256 = "1xncc6212981fp7fm3381l95h397m8f4li0ksv236a8bmpl62vd3";
        })
      ];
    });
  };
in with final.lib; rec {
  emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope' override;
  emacsPackages = prev.emacsPackages.overrideScope' override;
}
