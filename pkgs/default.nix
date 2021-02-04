final: prev: let
  inherit (final) recurseIntoAttrs;
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

  emacsPackagesFor = emacs: recurseIntoAttrs ((prev.emacsPackagesFor emacs).overrideScope' emacsOverride);

  dgit = prev.callPackage ./applications/version-management/dgit { };

  dejavu_nerdfont = prev.callPackage ./data/fonts/dejavu-nerdfont { };

  dotnetPackages = recurseIntoAttrs (prev.dotnetPackages.override { overrides = dotnetOverride; });

  electronmail = prev.callPackage ./applications/networking/mailreaders/electronmail { };

  fetchdarcs = prev.callPackage ./build-support/fetchdarcs { };

  cfcli = prev.callPackage ./applications/misc/cfcli { };

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

  mactelnet = prev.callPackage ./applications/networking/mactelnet { };

  miraclecast = prev.callPackage ./os-specific/linux/gnome-network-displays/default.nix { };

  nodePackages = recurseIntoAttrs prev.nodePackages;

  pleroma = prev.callPackage ./servers/pleroma { };

  pure = prev.callPackage ./shells/zsh/pure { };

  rPackages = recurseIntoAttrs prev.rPackages;

  shflags = prev.callPackage ./tools/misc/shflags { };

  taiwins = prev.stdenv.mkDerivation { name = "taiwins"; src = prev.fetchgit { url = "https://github.com/taiwins/taiwins"; rev = "722503b9eed47a8cc2dd53dd0b5b03d2f0a50c3f"; fetchSubmodules = true; sha256 = "l1DKuH8Q2Mp3NAFqe7weHPeXwh1Ux9eJ+f4gLE0qmNA="; }; nativeBuildInputs = with prev; [ meson pkgconfig libxkbcommon wayland cairo cmake udev librsvg wayland-protocols lua5_3 dbus libdrm libinput mesa pam ninja pixman xorg.xcbutil xorg.libxcb xorg.xcbutilwm xorg.xcbutilerrors ]; CFLAGS = [ "-Wno-error=unused-result" "-Wno-error=maybe-uninitialized" "-Wno-error=stringop-overflow=" ]; };

  twitterpub = prev.callPackage ./servers/twitterpub { };

  velox = prev.callPackage ./applications/window-managers/velox { };

  vervis = prev.callPackage ./applications/version-management/vervis { inherit fetchdarcs; };

  wgvanity = prev.callPackage ./tools/system/wgvanity { };

  wold = prev.callPackage ./applications/networking/wold { };

  yacy = prev.callPackage ./servers/yacy { };
}
