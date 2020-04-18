final: prev: {
  sddm-chili =
    prev.callPackage ./applications/display-managers/sddm/themes/chili { };

  emacsPackages =
    prev.emacsPackages // (prev.callPackage ./applications/editors/emacs-modes { });

  dgit = prev.callPackage ./applications/version-management/dgit { };

  dejavu_nerdfont = prev.callPackage ./data/fonts/dejavu-nerdfont { };

  matrix-construct = prev.callPackage ./servers/matrix-construct { };

  pure = prev.callPackage ./shells/zsh/pure { };

  weechat-matrix = prev.callPackage ./applications/networking/irc/weechat/plugins/matrix.nix { };
}
