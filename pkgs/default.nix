final: prev: let
  inherit (prev.lib) recurseIntoAttrs;
  emacsOverride = self: super: prev.callPackage ./applications/editors/emacs-modes { };
  dotnetOverride = {
    azure-functions-core-tools = prev.callPackage ./development/dotnet-modules/azure-functions-core-tools { };
  };
in rec {
  bottom = prev.callPackage ./tools/system/bottom { };

  emacsPackages = recurseIntoAttrs (prev.emacsPackages.overrideScope' emacsOverride);
  emacsPackagesFor = emacs: recurseIntoAttrs ((prev.emacsPackagesFor emacs).overrideScope' emacsOverride);

  dgit = prev.callPackage ./applications/version-management/dgit { };

  dejavu_nerdfont = prev.callPackage ./data/fonts/dejavu-nerdfont { };

  dotnetPackages = recurseIntoAttrs (prev.dotnetPackages.override { overrides = dotnetOverride; });

  electronmail = prev.callPackage ./applications/networking/mailreaders/electronmail { };

  fetchdarcs = prev.callPackage ./build-support/fetchdarcs { };

  flarectl = prev.callPackage ./applications/misc/flarectl { };

  fsnoop = prev.callPackage ./tools/misc/fsnoop { };

  git-get = prev.callPackage ./applications/version-management/git-get { };

  git-pr-mirror = prev.callPackage ./applications/version-management/git-and-tools/git-pr-mirror { };

  git-remote-ipfs = prev.callPackage ./applications/version-management/git-remote-ipfs { };

  greetd = prev.callPackage ./applications/display-managers/greetd { };

  haskellPackages = recurseIntoAttrs prev.haskellPackages;

  ipfscat = prev.callPackage ./applications/misc/ipfscat { };

  idrisPackages = recurseIntoAttrs prev.idrisPackages;

  lispPackages = recurseIntoAttrs prev.lispPackages;

  matrix-appservice-irc = prev.callPackage ./servers/matrix-appservice-irc { };

  miraclecast = prev.callPackage ./os-specific/linux/gnome-network-displays/default.nix { };

  mx-puppet-discord = prev.callPackage ./servers/mx-puppet-discord { };

  nyxt = prev.callPackage ./applications/networking/browsers/nyxt { };

  nodePackages = recurseIntoAttrs prev.nodePackages;

  pleroma = prev.callPackage ./servers/pleroma { };

  pure = prev.callPackage ./shells/zsh/pure { };

  rPackages = recurseIntoAttrs prev.rPackages;

  sddm-chili = prev.callPackage ./applications/display-managers/sddm/themes/chili { };

  shflags = prev.callPackage ./tools/misc/shflags { };

  twitterpub = prev.callPackage ./servers/twitterpub { };

  velox = prev.callPackage ./applications/window-managers/velox { };

  vervis = final.stable.callPackage ./applications/version-management/vervis { inherit fetchdarcs; };

  yacy = prev.callPackage ./servers/yacy { };
}
