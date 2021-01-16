final: prev: let
  inherit (final.lib) recurseIntoAttrs;
  emacsOverride = self: super: {
    _0xc = null;
    _2048-game = null;
    _4clojure = null;
    at = null;
    emacs-libvterm = null;
    emacsClangCompleteAsync = null;
    term-plus = null;
    term-plus-key-intercept = null;
    term-plus-mux = null;
    xml-plus = null;
  } // prev.callPackage ./applications/editors/emacs-modes { };
  dotnetOverride = {
    azure-functions-core-tools = prev.callPackage ./development/dotnet-modules/azure-functions-core-tools { };
  };
  haskellOverride = self: super: (
    final.lib.mapAttrs (_: p: if (p.meta.broken or false)
      then prev.haskell.lib.markUnbroken p
      else p) super
  ) // {};
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

  guix-ns = prev.callPackage ./tools/misc/guix-ns { };

  haskellPackages = recurseIntoAttrs (prev.haskellPackages.override { overrides = haskellOverride; });

  ipfscat = prev.callPackage ./applications/misc/ipfscat { };

  idrisPackages = recurseIntoAttrs prev.idrisPackages;

  lispPackages = recurseIntoAttrs prev.lispPackages;

  miraclecast = prev.callPackage ./os-specific/linux/gnome-network-displays/default.nix { };

  nodePackages = recurseIntoAttrs prev.nodePackages;

  pleroma = prev.callPackage ./servers/pleroma { };

  pure = prev.callPackage ./shells/zsh/pure { };

  rPackages = recurseIntoAttrs prev.rPackages;

  shflags = prev.callPackage ./tools/misc/shflags { };

  twitterpub = prev.callPackage ./servers/twitterpub { };

  velox = prev.callPackage ./applications/window-managers/velox { };

  vervis = prev.callPackage ./applications/version-management/vervis { inherit fetchdarcs; };

  yacy = prev.callPackage ./servers/yacy { };
}
