final: prev: {
  sddm-chili =
    prev.callPackage ./applications/display-managers/sddm/themes/chili { };

  emacsPackages =
    prev.emacsPackages // (prev.callPackage ./applications/editors/emacs-modes { });

  dgit = prev.callPackage ./applications/version-management/dgit { };

  dejavu_nerdfont = prev.callPackage ./data/fonts/dejavu-nerdfont { };

  flarectl = prev.callPackage ./applications/misc/flarectl { };

  matrix-appservice-irc = prev.callPackage ./servers/matrix-appservice-irc { };

  matrix-construct = prev.callPackage ./servers/matrix-construct { };

  mx-puppet-discord = prev.callPackage ./servers/mx-puppet-discord { };

  pleroma = prev.callPackage ./servers/pleroma { };

  pure = prev.callPackage ./shells/zsh/pure { };

  yacy = prev.callPackage ./servers/yacy { };
}
