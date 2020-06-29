final: prev: let
  inherit (prev.lib) recurseIntoAttrs;
  emacsOverride = self: super: prev.callPackage ./applications/editors/emacs-modes { };
  haskellOverride = self: super: rec {
    hayland = self.callPackage ./development/haskell-modules/haskell-wayland { };
    xkbcommon = self.callPackage ./development/haskell-modules/haskell-xkbcommon { };
    hfuse = self.callPackage ./development/haskell-modules/hfuse { };
    hsroots = self.callPackage ./development/haskell-modules/hsroots { };
    input = self.callPackage ./development/haskell-modules/libinput { };
    waymonad-scanner = self.callPackage ./development/haskell-modules/waymonad-scanner { };
  };
in {
  emacsPackages = recurseIntoAttrs (prev.emacsPackages.overrideScope' emacsOverride);
  emacsPackagesFor = emacs: recurseIntoAttrs ((prev.emacsPackagesFor emacs).overrideScope' emacsOverride);

  dgit = prev.callPackage ./applications/version-management/dgit { };

  dejavu_nerdfont = prev.callPackage ./data/fonts/dejavu-nerdfont { };

  electronmail = prev.callPackage ./applications/networking/mailreaders/electronmail { };

  flarectl = prev.callPackage ./applications/misc/flarectl { };

  fsnoop = prev.callPackage ./tools/misc/fsnoop { };

  haskellPackages = recurseIntoAttrs (prev.haskellPackages.override { overrides = haskellOverride; });

  ipfscat = prev.callPackage ./applications/misc/ipfscat { };

  idrisPackages = recurseIntoAttrs prev.idrisPackages;

  lispPackages = recurseIntoAttrs prev.lispPackages;

  matrix-appservice-irc = prev.callPackage ./servers/matrix-appservice-irc { };

  matrix-construct = prev.callPackage ./servers/matrix-construct { };

  mx-puppet-discord = prev.callPackage ./servers/mx-puppet-discord { };

  next = prev.callPackage ./applications/networking/browsers/next { };

  nodePackages = recurseIntoAttrs prev.nodePackages;

  pleroma = prev.callPackage ./servers/pleroma { };

  pure = prev.callPackage ./shells/zsh/pure { };

  rPackages = recurseIntoAttrs prev.rPackages;

  sddm-chili =
    prev.callPackage ./applications/display-managers/sddm/themes/chili { };

  shflags = prev.callPackage ./tools/misc/shflags { };

  yacy = prev.callPackage ./servers/yacy { };

  waymonad = final.haskellPackages.callPackage ./applications/window-managers/waymonad { };
}
