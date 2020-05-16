final: prev: {
  emacsPackages = with prev; emacsPackages // (callPackage ./applications/editors/emacs-modes { });

  arm-adb = prev.callPackage ./applications/misc/arm-adb { };

  dgit = prev.callPackage ./applications/version-management/dgit { };

  dejavu_nerdfont = prev.callPackage ./data/fonts/dejavu-nerdfont { };

  electronmail = prev.callPackage ./applications/networking/mailreaders/electronmail { };

  flarectl = prev.callPackage ./applications/misc/flarectl { };

  fsnoop = prev.callPackage ./tools/misc/fsnoop { };

  matrix-appservice-irc = prev.callPackage ./servers/matrix-appservice-irc { };

  matrix-construct = prev.callPackage ./servers/matrix-construct { };

  mx-puppet-discord = prev.callPackage ./servers/mx-puppet-discord { };

  next = prev.callPackage ./applications/networking/browsers/next { };

  pleroma = prev.callPackage ./servers/pleroma { };

  pure = prev.callPackage ./shells/zsh/pure { };

  sddm-chili =
    prev.callPackage ./applications/display-managers/sddm/themes/chili { };

  shflags = prev.callPackage ./tools/misc/shflags { };

  yacy = prev.callPackage ./servers/yacy { };
}
