#+title: NixOS System Configuration
#+author: bqv
#+email: nixos@fron.io
#+options: toc:nil num:nil

#+BEGIN_SRC nix
{
  description = "A highly structured configuration database.";

  inputs = {
    priv.url = "hg+ssh://bao@delta/../../srv/hg/nixpriv";

    master.url = "github:nixos/nixpkgs/master";               #|.
    staged.url = "github:nixos/nixpkgs/staging";              #| |-- Nix
    small.url  = "github:nixos/nixpkgs/nixos-unstable-small"; #| |--   pkgs
    large.url  = "github:nixos/nixpkgs/nixos-unstable";       #|'

    rel2009.url = "github:nixos/nixpkgs/nixos-20.09";         #| Stable
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03";         #| Old Stable
    rel1909 = { url = "github:nixos/nixpkgs/nixos-19.09"; flake = false; };
    rel1903 = { url = "github:nixos/nixpkgs/nixos-19.03"; flake = false; };
    rel1809 = { url = "github:nixos/nixpkgs/nixos-18.09"; flake = false; };
    rel1803 = { url = "github:nixos/nixpkgs/18.03"; flake = false; };
    pr75800.url = "github:nixos/nixpkgs/517b290754f6a7cc487ce11932a8b750f868324d"; #|\ Pull
    pr78810.url = "github:happy-river/nixpkgs/fe73376fcfe2eb1f72ab4ea52ad3bb0a12adc8d3"; #|\ Pull
    pr93659.url = "github:ju1m/nixpkgs/security.pass";                             #|/ Reqs
    pr99188.url = "github:atemu/nixpkgs/giara-init";                               #||
    pr96368.url = "github:islandusurper/nixpkgs/lbry-desktop";                     #||

    nix.url = "github:nixos/nix/progress-bar";                #|- Nix
    nix-ipfs.url = "github:obsidiansystems/nix/ipfs-develop"; #|  ^^^IPFS

    dwarffs.url = "github:edolstra/dwarffs";         #|- Dwarffs
    dwarffs.inputs.nix.follows = "/nix";             #|
    dwarffs.inputs.nixpkgs.follows = "/nix/nixpkgs"; #|

    home.url = "github:nix-community/home-manager"; #|- Home-manager
    home.inputs.nixpkgs.follows = "/master";        #|

    naersk.url = "github:nmattia/naersk";      #|- Naersk
    naersk.inputs.nixpkgs.follows = "/master"; #|

    guix.url = "github:emiller88/guix";      #|- Guix
    guix.inputs.nixpkgs.follows = "/master"; #|

    construct.url = "github:matrix-construct/construct"; #|- Construct
    construct.inputs.nixpkgs.follows = "/large";         #|

    hydra.url = "github:nixos/hydra/f64230b45edf07d1"; #|- Hydra

    apparmor.url = "github:bqv/apparmor-nix"; #|- Apparmor

    emacs.url = "github:nix-community/emacs-overlay"; # Emacs-overlay

    lisp.url = "github:nix-lisp/lisp-overlay"; # Lisp-overlay

    devshell.url = "github:numtide/devshell"; # Devshell

    nyxt.url = "github:atlas-engineer/nyxt"; #|- Nyxt
    nyxt.inputs.nixpkgs.follows = "/master"; #|

    wayland.url = "github:colemickens/nixpkgs-wayland"; #|- Nixpkgs-wayland
    wayland.inputs.nixpkgs.follows = "/small";          #|

    agenix.url = "github:ryantm/agenix";          #|- AgeNix
    agenix.inputs.nixpkgs.follows = "/small";     #|
    agenix.inputs.flake-utils.follows = "/utils"; #|

    haskell.url = "github:input-output-hk/haskell.nix"; # Haskell.nix
    utils.url = "github:numtide/flake-utils";           # Flake-utils
    hardware.url = "github:nixos/nixos-hardware";       # Nixos-hardware

    xontribs.url = "github:bqv/xontribs"; #|- Xontribs
    xontribs.inputs = {
      nixpkgs.follows = "/master";
      prompt-bar = { url = "github:anki-code/xontrib-prompt-bar/68b3487e156ed3dce80578ebe552b6afa94c7eb8"; flake = false; };
      pipeliner = { url = "github:anki-code/xontrib-pipeliner/daccb6c8a67bbda799dfa2d6d8d829b5e9151c92"; flake = false; };
    };

    taiwins = {
      type = "git";
      url = "https://github.com/taiwins/taiwins";
      ref = "xwayland";
      submodules = true;
      flake = false;
    };

    processmgmt = { url = "github:svanderburg/nix-processmgmt"; flake = false; };  # ProcessMgmt
    hnix-overlay = { url = "github:haskell-nix/hnix"; flake = false; };            # Hnix
    impermanence = { url = "github:nix-community/impermanence"; flake = false; };  # Impermanence
    mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };          # Nixpkgs-mozilla
    baduk = { url = "github:dustinlacewell/baduk.nix"; flake = false; };           # Baduk
    snack = { url = "github:nmattia/snack"; flake = false; };                      # Snack
    napalm = { url = "github:nmattia/napalm"; flake = false; };                    # Napalm
    statichask = { url = "github:nh2/static-haskell-nix"; flake = false; };        # Static Haskell
    anki-sync = { url = "github:ankicommunity/anki-sync-server/125f7bb1"; flake = false; }; # Anki Server
    conix = { url = "github:thenerd247/conix"; flake = false; };
    matrix-nio = { url = "github:poljar/matrix-nio/98f0c244"; flake = false; };
    weechat-matrix = { url = "github:poljar/weechat-matrix/d4158416"; flake = false; };
    sqlcmdline = { url = "github:sebasmonia/sqlcmdline"; flake = false; };
    fish-bass = { url = "github:edc/bass"; flake = false; };
    spotify-adblock = { url = "github:x0uid/spotifyadblock"; flake = false; };
    bottom = { url = "github:clementtsang/bottom/0.4.5"; flake = false; };
    twitterpub = { url = "github:bqv/twitterpub"; flake = false; };
    zsh-pure = { url = "github:sindresorhus/pure"; flake = false; };
    fsnoop = { url = "github:jeffwalter/fsnoop"; flake = false; };
    shflags = { url = "github:kward/shflags"; flake = false; };
    git-remote-ipfs = { url = "github:bqv/git-remote-ipfs"; flake = false; };
    git-get = { url = "github:grdl/git-get"; flake = false; };
    git-pullrequest = { url = "github:google/git-pull-request-mirror"; flake = false; };
    dgit = { url = "github:quorumcontrol/dgit/v0.0.14-alpha"; flake = false; };
    wld = { url = "github:michaelforney/wld"; flake = false; };
    swc = { url = "github:bqv/swc"; flake = false; };
    velox = { url = "github:michaelforney/velox"; flake = false; };
    st-wl = { url = "github:michaelforney/st"; flake = false; };
    dmenu = { url = "github:michaelforney/dmenu"; flake = false; };
    emacs-bitwarden = { url = "github:seanfarley/emacs-bitwarden"; flake = false; };
    ivy-exwm = { url = "github:pjones/ivy-exwm"; flake = false; };
    flycheck-purescript = { url = "github:bsermons/flycheck-purescript"; flake = false; };
    eterm-256color = { url = "github:dieggsy/eterm-256color"; flake = false; };
    envrc = { url = "github:purcell/envrc"; flake = false; };
    emacsbridge = { url = "github:aardsoft/emacsbridge"; flake = false; };
    font-lock-ext = { url = "github:sensorflo/font-lock-ext"; flake = false; };
    sln-mode = { url = "github:sensorflo/sln-mode"; flake = false; };
    emacs-ffi = { url = "github:tromey/emacs-ffi"; flake = false; };
    explain-pause-mode = { url = "github:lastquestion/explain-pause-mode"; flake = false; };
    gnome-network-displays = { url = "git+https://gitlab.gnome.org/gnome/gnome-network-displays"; flake = false; };
    emacs-webkit = { url = "github:akirakyle/emacs-webkit"; flake = false; };
    giara = { url = "git+https://gitlab.gnome.org/world/giara"; flake = false; };
    ini2json = { url = "github:anubisss/ini2json"; flake = false; };
    mfs-replace-root = { url = "github:hsanjuan/mfs-replace-root"; flake = false; };
    brig = { url = "github:sahib/brig/develop"; flake = false; };
    emacs-straight = { url = "github:raxod502/straight.el"; flake = false; };
    cloudflare-cli = { url = "github:danielpigott/cloudflare-cli"; flake = false; };
    wgvanity = { url = "github:warner/wireguard-vanity-address"; flake = false; };
    wold = { url = "github:pauliuszaleckas/wold"; flake = false; };
    mactelnet = { url = "github:haakonnessjoen/mac-telnet"; flake = false; };
  };

  outputs = inputs: with builtins; let
    allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];

    # Nixos Config
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };

    patchNixpkgs = basePkgs: let
      pullReqs = map (meta: {
        url = meta.url or "https://github.com/nixos/nixpkgs/pull/${toString meta.id}.diff";
        name = "nixpkgs-pull-request-${toString meta.id}";
        inherit meta;
        sha256 = meta.hash;
      }) [
        {
          description = "nixos/anbox: use mainline drivers when available";
          id = 102341; hash = "68IzjRPbRuDQ9Lk8WHbYTbxvTr0pHH3wIuSh7ISaqiQ=";
        }
        {
          description = "nixos/nat: substitute iptables for compat under nftables";
          id = 85462; hash = "vU53uZUhhO6U2RGElAnZqAy3KForw/yyPiU5Rg1hL74=";
        }
       #{
       #  description = "emacs: disable trampoline generation when installing packages";
       #  id = 109370; hash = "uVlMZ92myOvB64QIC2MZMmBZwpMrB+qxa48W86oVqZU=";
       #}
      ];
      patches = [
       #(basePkgs.fetchurl {
       #  name = "grub-use-xkb-config";
       #  url = "https://github.com/NixOS/nixpkgs/compare/master...mdevlamynck:4a709715e3de83bfc34b880b8044af41a558316e.diff";
       #  sha256 = "1bkbr2znnwi5yc210vhnj638i1ls1w35sdhh3hfh6fnxlbjlmfbn";
       #})
      ] ++ map basePkgs.fetchpatch pullReqs;
      patchedTree = basePkgs.applyPatches {
        name = "nixpkgs-patched";
        src = basePkgs.path;
        inherit patches;
        postPatch = ''
          patch=$(printf '%s\n' ${builtins.concatStringsSep " " (map (p: p.outputHash or (builtins.baseNameOf p)) patches)} |
          sort | sha256sum | cut -c -7)
          echo "+patch-$patch" >.version-suffix
        '';
      };

      import_nixpkgs = args: import patchedTree ({ inherit (basePkgs) system config overlays; } // args);
    in import_nixpkgs {} // patchedTree // {
      lib = (import_nixpkgs {}).lib;
      legacyPackages = basePkgs.lib.genAttrs allSystems (system: _: import_nixpkgs { inherit system; });
    };

    channels = with inputs; {
      pkgs = small;       # For packages
      modules = master;   # For nixos modules
      lib = master;       # For flake-wide lib
    }; inherit (channels.lib) lib; # this ^

    # Packages for nixos configs
    pkgsForSystem = system: patchNixpkgs (import channels.pkgs rec {
      inherit system config;
      overlays = builtins.map (o: o // { value = lib.fix o.__functor; }) [{
        # Project each overlay through recursive subsets
        __functor = self: final: prev: let
          overlaySets = lib.mapAttrs' (ident: overlay: {
            name = "with" + (with lib.strings; let
              ch = stringToCharacters ident;
              ch' = [(toUpper (head ch))] ++ tail ch;
            in concatStringsSep "" ch');
            value = import prev.path {
              inherit (prev) system config;
              overlays = prev.overlays ++ [{
                value = _final: _prev: {
                  inherit overlay;
                  pkgsParent = final;
                } // overlay _final _prev;
                __functor = x: x.value;
                name = ident;
              }];
            };
          }) inputs.self.overlays;
        in overlaySets // {
          inherit overlaySets;
          overlay = self;
          overlaysBase = overlays;
          pkgsParent = prev // { overlays = []; };
          appendOverlays = extensions: prev.appendOverlays [{
            inherit extensions;
            value = lib.fold lib.composeExtensions (self: lib.id) extensions;
            __functor = x: _final: _prev: x.value _final _prev // {
              pkgsParent = final;
            };
            provides = builtins.listToAttrs (lib.imap0 (n: ext: {
              name = "_${toString n}";
              value = builtins.attrNames (ext {} {});
            }) extensions);
            name = "config";
          }];
          withPins = import prev.path {
            inherit (prev) system config;
            overlays = prev.overlays ++ [{
              # Flatten pkgs architecture superficially for convenient maintenance
              value = _final: _prev: let
                overlayPkgs = with final; {
                  # hack to prioritize config overlays
                  appendOverlays = exts: (final.appendOverlays exts).withPins;

                  # this is one light breeze away from infrec
                  inherit (withGuixFlake) guix;
                  inherit (withEmacsFlake.withSelfFlake.withEmacs) emacsPgtkGcc emacsPgtkGccClient emacsPgtkGccPackages;
                  inherit (withGiara) giara;
                  inherit (withLbry) lbry;
                  inherit (withCordless) cordless;
                  inherit (withMaster.withHnix) hnix;
                  inherit (withNix) nixFlakes nix-static nix-ipfs;
                  inherit (withInsecureSSL) epsxe;
                  inherit (withHydraFlake.withNix.withHydra) hydra hydra-unstable;
                  inherit (withApparmorFlake) apparmorRulesFromClosure;
                  iputils = iputils // { inherit (withApparmorFlake.iputils) apparmor; }; # shh it's fine
                  inetutils = inetutils // { inherit (withApparmorFlake.inetutils) apparmor; }; # shh it's fine
                  inherit (withNaersk.withSelfFlake) greetd;
                  inherit (withSelfFlake) velox electronmail;
                  dotnetPackages = dotnetPackages // {
                    inherit (withSelfFlake.dotnetPackages) azure-functions-core-tools;
                  };
                  haskellPackages = haskellPackages // {
                    inherit (withRel2009.haskellPackages) pointfree-fancy;
                  };
                  inherit (withSelfFlake) dejavu_nerdfont;
                  emacsPackagesFor = emacs: let
                    inherit (withEmacsFlake) emacsPackagesFor;
                    epkgs = withEmacsFlake.withSelfFlake.withEmacs.emacsPackagesFor emacs;
                  in (emacsPackagesFor emacs).overrideScope' (_: _: {
                    inherit (epkgs) bitwarden ivy-exwm emacs-webkit;
                    inherit (epkgs) flycheck-purescript eterm-256color;
                    inherit (epkgs) envrc emacsbridge font-lock-ext sln-mode;
                    inherit (epkgs) emacs-ffi explain-pause-mode weechat-patched;
                  });
                  inherit (withSelfFlake) git-pr-mirror git-remote-ipfs git-get ipfscat;
                  inherit (import path { inherit system; }) notmuch; # ouch
                  inherit (withSelfFlake) guix-ns twitterpub;
                  inherit (withConstructFlake.withConstruct) matrix-construct;
                  inherit (withSelfFlake) yacy;
                  inherit (withRel2003.withSelfFlake) vervis;
                  inherit (withPr78810) mastodon;

                  inherit (withSelfFlake) cfcli dgit fsnoop pure shflags;
                  inherit (withIni2json) ini2json;
                  inherit (withSelfFlake.pleroma) pleroma_be pleroma_fe masto_fe;
                  inherit (withNix.withDwarffsFlake) dwarffs;
                  inherit (withNaersk) naersk;
                  inherit (withXonsh.withXontribsFlake) xonsh;
                  inherit (withNyxt) nyxt;

                  inherit (withWeechat) weechatScripts;
                  inherit (withRel2003) bcachefs-tools; # to match kernel ver
                  inherit (withNaersk.withSelfFlake) wgvanity wold mactelnet;
                  inherit (withNix.withSelfFlake) nix-bundle;

                 #inherit (withSmall) firefox firefox-unwrapped;
                 #inherit (withSmall) thunderbird obs-studio webkitgtk chromium qemu;
                  plasma5 = plasma5Packages;
                  inherit (libsForQt5) kdeFrameworks;
                };
              in overlaySets // overlayPkgs // {
                inherit overlaySets overlayPkgs;
                pkgsParent = final;
              };
              __functor = x: x.value;
              name = "pins";
            }];
          };
        };
        name = "index";
      }];
    });

    forAllSystems = f: lib.genAttrs allSystems (system: f {
      inherit system;
      pkgs = (pkgsForSystem system).withPins;
      inherit (pkgs) lib;
    });

    inputMap = let
      tryGetValue = res: if res.success then res.value else null;
    in {
      n1 = lib.mapAttrsToList lib.nameValuePair inputs;

      n2 = let
        s0 = inputs // { self = {}; };
        s1 = lib.mapAttrs (k: v: v.inputs) (lib.filterAttrs (k: v: v ? inputs) s0);
        s2 = lib.mapAttrsRecursiveCond (s: !(s ? outPath)) (n: i: lib.nameValuePair (lib.concatStringsSep "." n) i) s1;
      in lib.concatMap lib.attrValues (lib.attrValues s2);

      n3 = lib.const [] (let # broken (but why?)
        s0 = inputs // { self = {}; };
        s1 = lib.mapAttrs (k: v: v.inputs) (lib.filterAttrs (k: v: v ? inputs) s0);
        s2 = lib.mapAttrsRecursiveCond (s: !(s ? outPath)) (n: i: lib.nameValuePair (lib.concatStringsSep "." n) i) s1;
        s3 = lib.concatMap lib.attrValues (lib.attrValues s2);
        s4 = lib.mapAttrs (k: v: v.inputs) (lib.filterAttrs (k: v: v ? inputs) (lib.listToAttrs s3));
        s5 = lib.mapAttrsRecursiveCond (s: !(s ? outPath)) (n: i: lib.nameValuePair (lib.concatStringsSep "." n) i) s4;
        s6 = lib.concatMap lib.attrValues (lib.attrValues s5);
      in tryGetValue (builtins.tryEval (lib.concatMap lib.attrValues (lib.attrValues s6))));
    };

    inherit (inputs.self.passthru) secrets;
  in {
    nixosConfigurations = builtins.mapAttrs (host: node: {
      config = node.configuration;
    }) inputs.self.defaultPackage.x86_64-linux.config.nodes;

    homeConfigurations = lib.genAttrs (builtins.attrNames inputs.self.nixosConfigurations)
      (host: inputs.self.nixosConfigurations.${host}.config.home-manager.users) //
    {
      epsilon = forAllSystems ({ pkgs, system, ... }:
        let
          inherit (pkgs) pkgsStatic nix nix-static termonad-with-packages;
          flakeModules = import ./modules/home-manager.nix;
        in inputs.home.lib.homeManagerConfiguration rec {
          pkgs = pkgsStatic // {
            nixFlakes = nix;
            termonad = termonad-with-packages;
          };
          configuration = {
            _module.args = rec {
              pkgsPath = pkgs.path;
              inherit pkgs;
              nixosConfig = {
                services.aria2.rpcSecret = "";
                networking.hostName = "epsilon";
              };
              flake = inputs.self;
            };
            nixpkgs = {
              inherit config system;
            };
            home.packages = with pkgs; [ nixFlakes termonad ];
            imports = flakeModules ++ [ ./users/aion.nix ];
          };
          inherit system;
          homeDirectory = "/home/${username}";
          username = "aion";
        }
      );
    };

    legacyPackages = forAllSystems ({ pkgs, ... }: pkgs);

    overlay = import ./pkgs;

    overlays = let
      diffTrace = left: right: string: value: if left != right then trace string value else value;
      silentKeys = [ "config" "system" "path" "stdenv" "lib" ];

      channelOverlay = { flake, branch }: { ${flake} = final: prev: let
        mapWarn = mapAttrs (k: v: if lib.elem k silentKeys then v
          else if lib.elem k ([ "overlaySets" ] ++ builtins.attrNames prev.overlaySets) then mapWarn v
          else diffTrace (baseNameOf inputs.${flake}) (baseNameOf prev.path) "pkgs.${k} pinned to nixpkgs/${branch}" v);
      in
        mapWarn (import inputs.${flake} { inherit (prev) config system; overlays = [(builtins.head prev.overlays)]; });
      };
      flakeOverlay = { flake, name }: { ${flake} = final: prev: let
        mapWarn = mapAttrs (k: v: if lib.elem k silentKeys then v
          else if lib.elem k ([ "overlaySets" ] ++ builtins.attrNames prev.overlaySets) then mapWarn v
          else diffTrace (baseNameOf inputs.${flake}) (baseNameOf prev.path) "pkgs.${k} pinned to ${name}" v);
      in
        mapWarn inputs.${flake}.legacyPackages.${prev.system};
      };
      inputsOverlay = builtins.listToAttrs (builtins.filter (v: v != null)
        (builtins.map (name: if inputs.${name} ? overlay then {
          name = "${name}Flake";
          value = inputs.${name}.overlay;
        } else null) (builtins.attrNames inputs)));
      configOverlay = config: final: prev: import prev.path {
        inherit (prev) system;
        overlays = [(builtins.head prev.overlays)];
        config = prev.config // config;
      };
    in lib.fold (lib.mergeAttrsNoOverride {}) {} [
      {
        broken = configOverlay { allowBroken = true; };
        insecureSSL = configOverlay { permittedInsecurePackages = [ "openssl-1.0.2u" ]; };
        sources = final: prev: inputs;
        lib = final: prev: { inherit lib; };
      }
      {
        mozilla = final: prev: import inputs.mozilla final prev;
        naersk = final: prev: { naersk = inputs.naersk.lib.${prev.system}; };
        snack = final: prev: { snack = final.callPackage "${inputs.snack}/snack-lib"; };
        napalm = final: prev: { napalm = final.callPackage inputs.napalm; };
        nix = final: prev: let inherit (prev) system; in rec {
          nixFlakes = nix;
          nixUnstable = nix;
          nix = inputs.nix.packages.${system}.nix.overrideAttrs (drv: {
            patches = (drv.patches or []) ++ [
              (final.fetchpatch {
                name = "libfetcher-file.patch";
                url = "https://github.com/nixos/nix/pull/4153.diff";
                sha256 = "JfcswqOG0V5qlolxxYFOpqXJgENC4Adfk4J8r//tgfA=";
              })
            ];
            passthru = {
              inherit (inputs.nix.packages.${system}.nix) perl-bindings;
            };
          });
          nix-static = inputs.nix.packages.${system}.nix-static or null;
          nix-ipfs = inputs.nix-ipfs.packages.${system}.nix;
         #nix-ipfs-static = inputs.nix-ipfs.packages.${system}.nix-static;
        };
        hydra = final: prev: {
          hydra-unstable = final.hydra;
        };
        construct = final: prev: {
          riot-web = final.element-web;
          matrix-construct = (final.callPackage "${inputs.construct}/default.nix" { pkgs = final; }).overrideAttrs (_: {
            EXTRA_CXXFLAGS = "-mabm -mbmi";
            patchPhase = '' sed '/RB_INC_EXECUTION/d' -i ./include/ircd/stdinc.h '';
            preAutoreconf = let
              VERSION_COMMIT_CMD = "git rev-parse --short HEAD";
              VERSION_BRANCH_CMD = "git rev-parse --abbrev-ref HEAD";
              VERSION_TAG_CMD = "git describe --tags --abbrev=0 --dirty --always --broken";
              VERSION_CMD = "git describe --tags --always";
            in ''
              substituteInPlace configure.ac --replace "${VERSION_COMMIT_CMD}" "echo ${inputs.construct.rev}"
              substituteInPlace configure.ac --replace "${VERSION_BRANCH_CMD}" "echo master"
              substituteInPlace configure.ac --replace "${VERSION_TAG_CMD}" "echo ${inputs.construct.rev}"
              substituteInPlace configure.ac --replace "${VERSION_CMD}" "echo ${inputs.construct.rev}"
            '';
            src = builtins.toPath "${inputs.construct}/.";
          });
        };
      }
     #(flakeOverlay { flake = "lg400"; name = "delta/system-400-link"; })
      (channelOverlay { flake = "master"; branch = "master"; })
      (channelOverlay { flake = "staged"; branch = "staging"; })
      (channelOverlay { flake = "small"; branch = "nixos-unstable-small"; })
      (channelOverlay { flake = "large"; branch = "nixos-unstable"; })
      (channelOverlay { flake = "rel2009"; branch = "nixos-20.09"; })
      (channelOverlay { flake = "rel2003"; branch = "nixos-20.03"; })
      (channelOverlay { flake = "rel1909"; branch = "nixos-19.09"; })
      (channelOverlay { flake = "rel1903"; branch = "nixos-19.03"; })
      (channelOverlay { flake = "rel1809"; branch = "nixos-18.09"; })
      (channelOverlay { flake = "rel1803"; branch = "nixos-18.03"; })
      (channelOverlay { flake = "pr78810"; branch = "feature/mastodon"; })
      (listToAttrs (map
        (name: {
          name = lib.removeSuffix ".nix" name;
          value = import (./overlays + "/${name}") inputs;
        })
        (builtins.filter
          (file: lib.hasSuffix ".nix" file || file == "default.nix")
          (attrNames (readDir ./overlays)))))
      inputsOverlay
    ];

    packages = forAllSystems ({ pkgs, ... }: lib.filterAttrs (_: p:
      (lib.isDerivation p) && (p.meta.broken or null) != true
    ) pkgs.overlayPkgs);

    defaultPackage = forAllSystems ({ pkgs, system, ... }: let
      deployment = import ./deploy {
        nixpkgs = patchNixpkgs (channels.modules.legacyPackages.${system});
        deploySystem = system; # By habit, system is deployer, platform is target
      } ({ config, lib, ... }: let
        inherit (config) nodes;
      in {
        defaults = { name, config, ... }: let
          evalConfig = import "${patchNixpkgs pkgs}/nixos/lib/eval-config.nix";

          getPlatform = with lib.modules; { modules, specialArgs, ... }: let
            args = { config = null; options = null; inherit lib; } // specialArgs;
          in (mergeModules [] (collectModules "" modules args)).matchedOptions.platform.value;

          nixos = with inputs.self.nixosModules; let
            base = hosts.${system}.${name} or null;
            platform = getPlatform base;
          in if base == null then hosts.${system}.image else hosts.${platform}.${name};

          vmsystem = { modules, pkgs, specialArgs, ... }: {
            system.build.vm = (evalConfig {
              inherit specialArgs;
              inherit (pkgs) system;
              modules = modules ++ [
                (import "${channels.modules}/nixos/modules/virtualisation/qemu-vm.nix")
              ];
            }).config.system.build.toplevel;
          };

          linkage = { config, pkgs, ... }: let
            systems = builtins.mapAttrs (host: _: with inputs.self;
              let platform = getPlatform nixosModules.hosts.${system}.${host};
              in defaultPackage.${platform}.config.nodes.${host}.configuration
            ) nodes;
          in {
            options.system.linkOtherSystems = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Whether to link other flake nodes to the system derivation.";
            };

            # Link raw hosts on each host (non-recursively)
            config.system = {
              extraSystemBuilderCmds = lib.mkIf config.system.linkOtherSystems (''
                mkdir -p $out/flake/hosts

                # Link other hosts (nonrecursively)
                ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                  ln -s '${value.system.build.toplevel}' "$out/flake/hosts/${name}"
                '') (lib.mapAttrsToList lib.nameValuePair systems)}

                # Link host containers
                ${lib.concatMapStringsSep "\n" (host@{ name, value }: ''
                  mkdir -p $out/flake/container/${name}
                  ${lib.concatMapStringsSep "\n" (container@{ name, value }: ''
                    ln -s '${value.config.system.build.toplevel}' "$out/flake/container/${host.name}/${name}"
                  '') (lib.mapAttrsToList lib.nameValuePair value.containers)}
                '') (lib.mapAttrsToList lib.nameValuePair systems)}
              '');
            };
          };
        in {
          options = {
            configuration = lib.mkOption {
              type = lib.types.submoduleWith {
                inherit (nixos) modules;
              };
            };
          };

          config = {
            host = "root@${nixos.specialArgs.hosts.wireguard.ipv4.${name}}";

            configuration = {
              imports = [
               #linkage # TODO: figure out how to make this work
                vmsystem
              ];
              config = {
                secrets.baseDirectory = "/var/lib/secrets";
                _module.args = nixos.specialArgs;
              };
            };

            # Filter out "added to list of known hosts" spam from output
            deployScriptPhases.filter-known-hosts = lib.dag.entryBefore ["copy-closure"] ''
              # Remove known hosts spam
              pipeline -w { ${pkgs.gnugrep}/bin/grep --line-buffered -v "list of known hosts" }
              fdswap 1 2
              pipeline -w { ${pkgs.gnugrep}/bin/grep --line-buffered -v "list of known hosts" }
              fdswap 2 1
            '';

            # Git tag all systems and deployments
            deployScriptPhases.git-tag = let
              inherit (config.configuration) system;
            in lib.dag.entryAfter ["trigger-switch"] ''
              foreground {
                backtick -i -n systemstorepath { basename "${system.build.toplevel.outPath}" }
                importas -i systemstorepath systemstorepath
                define systempath "nix/store/''${systemstorepath}"
                importas -i id ID
                define systemnum "${name}/system-''${id}"
                fdswap 1 2
                foreground { echo Tagging $systempath }
                foreground { ${pkgs.git}/bin/git tag $systempath "${system.configurationRevision}" }
                foreground { echo Tagging $systemnum }
                foreground { ${pkgs.git}/bin/git tag $systemnum "${system.configurationRevision}" }
                exit 0
              }
            '';

            privilegeEscalationCommand = []; # already root

            successTimeout = lib.mkDefault 120;
            switchTimeout = lib.mkDefault 120;

            ignoreFailingSystemdUnits = true;
            systemSwitcherDir = "/nix/node/";

            dirty = ! (inputs.self ? rev);
          };
        };

        nodes = let
          hosts = builtins.attrNames inputs.self.nixosModules.hosts.${system};
        in (lib.genAttrs hosts (_: {})) // {
          delta.hasFastConnection = true; # it's local!
          delta.rollbackOnFailure = false; # accidentally rebooting this gets annoying
          image.enabled = false;
          zeta.panicAction = "false"; # we shouldn't reboot this carelessly
          zeta.hasFastConnection = true;
          zeta.successTimeout = 240; # Zeta seems very slow...
          zeta.switchTimeout = 240; # maybe due to wireguard reloading?
        };

        ssh.access = let
          collapse = attrs: lib.fold (a: b: a // b) {}
            (builtins.concatMap builtins.attrValues (builtins.attrValues
              (lib.mapAttrs (u: lib.mapAttrs (t: v: {
                "${u}-${t}" = v;
              })) attrs)));
        in lib.mkIf false {
          delta = {
            hostKeys = lib.genAttrs [ "rsa" "ecdsa" "ed25519" "dsa" ] (type:
              lib.removeSuffix "\n" (builtins.readFile "${secrets.keyDir}/deltassh/ssh_host_${type}_key.pub")
            );
            keys = collapse {
              bao = {
                rsa = {
                  publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQC/Ca5MUwmHMZt4sH/xLAq20X12xDj2bSqg88thxWLvpqnLpLEJmpasaHKRRh9O6E/NE0Zsn45dsTuvB9/otoDGdstYX2VZyi3UBIWp3BjxdbvmMrbhvpihiZl6rAFXU45/LM/4PvraLic2ZXuOGFqoLcnKDAvCATJsQfUGcEnM0YJVcFLjA4sSPHarSoSHFxZ90RXVmW8m5sfNCsMMo3jCMgE49zlWhcrKfp/FfpIlKMytTDdb+JE0JIsR+28oxJtL63wTbO9p2zFS8geNPVMfj8+Ge4bb0YWroBNGaMo3cHcdtquWWvTzZxWJu+AiSlZIWcP6aOhOoRiFrCWIYBP91dtARm2+OXIpUNAT+yPR+YQ4BAUNxYyBZO3hTH47cq2dC4NAQVcjuUb5VLoRAOAl+DzHSj7FNFGo49CxE/eDlhoXzmgP28N5tDktZYYstrr4j/KAZPYzGW8tB6s/VhhiCGFgZQXq2irH2UtU3Amr7cw0Wiiuyoe4x67dUB+sy3yT5jle/dLe0U9qgAiFTptix1QpjDaT8dFYQsi0kzXlSUEB05pmnCjM2n9eksdtF5kggLDNQ3VKH+LRN/rd5JKJsMJn9Lxr9nO3x+x+fhBurthumrDB2S3oqEOWQDxJ+JaZi5mshzZ2bB4lSXGB5aYjaBSk49EmlrNo8dUUXpZFT9mePx8BIC73XtoEsFgH9kpvHqHxOaBBO3wsxWekbS5bzi2KkBWxMAHq/fVV37iNaDHnldEn9DGDIQb6fQnjLkWbwNRBsdZxqfJGBqVpUNVbsS4BOFoa7yxnCZ6OgxzXpScoyeTxgM1nDALRnwee2d+3GrKHzE23Db6tIwUUeHEkcNwY2L6MW6Z2Onv2T9+V0IITe16EV+TcTc04DQ2XtnVEqUfqK5NZX0wpDuO0Bw0cJrbxJy3lk1PbnUnP/slfRgB4yOvkA0zRrer27EiyRsQe9QSfcmcpIK66+UncYqTFZ/qJFBdupn2ruYaDfCq/G8HyNsm7fXJLgsnGrAfEqUlBabbmLBdSvPqWpkMCjmmX7tMvTTWV5/IdFo0NVPQ7VBdZrRoPbYOQetZqr6S1tkKvpISrBKMRXXgaCcegnqWKqomZsFWcTnhyV0vCJM16IaNp++2rqw426hvpm2F+hhUzDa7CP/+HKvo0ucwC1u24CX75eT/pEP7WuINNMEx1y+/VxyeWscAK89NYa2YGBzqBEJ9+0QVFNYye/baaNn1dNEuPk49bkM7g3LTp8ae+J+5dAb3QsKMxL/bssFzAc+7M5+LFQDFpWtxkzkB5+X9HzMtVbsgDk9CDgn56OfkpzjFR7PeDUcKeFWeccWHRMOyXVX1daoUTeDIpESID2FnR bao@delta";
                  hasAccessTo.zeta.bao = true;
                };
                ecdsa = {
                  publicKey = "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAEY1DRzmt77NPp64/DEkuPwdOd5yzchRBDmZJBvu+ip42tV3c1FdDNs/7ictpROqfm/Nkh6D/mmbQV+MI2Aa1H66AC+8/wMJl/fkfSEeX6Z6thVM7oC6a2kXuJjlZNF5wQngTP5AWydEFacuZduW+Ir2++IVFyjtIVOR/lumtdzuvi5/A== bao@delta";
                  hasAccessTo.zeta.bao = true;
                };
                ed25519 = {
                  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvcvk1nLYImKqjhL8HdAb1sM2vXcEGu+rMZJ8XIG4H7 bao@delta";
                  hasAccessTo.zeta.bao = true;
                };
                gpg = {
                  publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDX40Dm37dDdxreqQxQ+bd9c9mNXfSqC5k3Rrf4iQHy7OdzsQD0qngHFdcq2dG0d4sYeHeyJGYHPbtwNYb0AED/zDVgfuiW6+xYSW+apg5Hc2EfktxxmVbRXqq8SoWUP9CtwJLIw0Q4vBiWBKT367VlXnWJ5ob8bwhfRlK7Sk4595Mm2Wp98sgMXdcQbFc+nQ+TYyYnkIYv63g3Lyxi8NQKWGwh2zspGl2rbDVJrtr196bSd3leZhD/v+YquqO2Saf9DZCmDMJmw4rUWZ84rkUZ0lB5eSICfQZAl5UxpnorWutLDxjuFOc/7H6iSGFRNjQ/fM8Jh2dxDm3XWxWp+E+F9q5Q9bsziL+zwxkAL2SvWULhmAALbLMqEC2qkPU9ccqRRBteGIRt/ix+J87lImz6Zp2UqSYFe2GpwP+NtMB8TyMaDUOni+L/NGw7o/EvAm7K7tH6OJW5vSC7e8P5lLc825SHlJdfn5L2aAfD+vr8rZv4L9uy1isJTMvVDRp4CLHwsw+xn4XfnfvsNtWNyVoAfi7NN3+jtCGlpNckj+5ylaQcgGuFbQUJ7jhsUGDYRSvX3wWaRBD94Pi+XS1jUTLyRmIbgcbiSeyzx2fZm1saQAb3MSF9yf0ibCJlTJ3JMLfqlDYP6Yl64bQ67T8QcYvDSUurqh+T6AN83SF6aHT3HQ== gpg";
                  hasAccessTo.zeta.bao = true;
                };
              };
            };
          };
          zeta = {
            hostKeys = lib.genAttrs [ "rsa" "ecdsa" "ed25519" "dsa" ] (type:
              lib.removeSuffix "\n" (builtins.readFile "${secrets.keyDir}/zetassh/ssh_host_${type}_key.pub")
            );
            keys = collapse {
            };
          };
        };

        vpn.networks = {
          # TBC
        };
      });
    in pkgs.runCommandLocal "deployment" {
      outputs = [ "out" "systems" ] ++ builtins.attrNames (
        lib.filterAttrs (_: n: n.enabled) deployment.config.nodes
      );
      passthru = deployment;
    } ''
      mkdir -p $(dirname $out)
      ln -s ${deployment.config.deployScript} $out
      ${lib.concatStringsSep "" (lib.mapAttrsToList (host: node: if node.enabled then ''
        ln -s ${node.nodeDeployScript} ''$${host}
        mkdir -p $systems
        ln -s ${node.configuration.system.build.toplevel} $systems/${host}
      '' else "") deployment.config.nodes)}
    '');

    apps = forAllSystems ({ pkgs, system, ... }: {
      epsilon = rec {
        type = "app";
        inherit (inputs.self.homeConfigurations.epsilon.${system}) activationPackage;
        program = (pkgs.writeShellScript "reconfigure-epsilon" ''
          echo Deploying ${activationPackage}
          export HOST=epsilon
          export NIX_SSHOPTS="-o StrictHostKeyChecking=no"
          nix copy --to ssh://$HOST '${activationPackage}' \
            && ssh $NIX_SSHOPTS $HOST -t \
              sh -c ". $HOME/.nix-profile/etc/profile.d/nix.sh && \
                exec ${activationPackage}/activate $@"
        '').outPath;
      };
      delta = rec {
        type = "app";
        inherit (inputs.self.nixosConfigurations.delta.config.system.build) toplevel;
        program = (pkgs.writeShellScript "test-delta" ''
          echo Deploying ${toplevel}
          exec doas ${toplevel}/bin/switch-to-configuration test $@
        '').outPath;
        bao = rec {
          type = "app";
          inherit (inputs.self.homeConfigurations.delta.bao.home) activationPackage;
          program = (pkgs.writeShellScript "test-delta-bao" ''
            echo Deploying ${activationPackage}
            exec ${activationPackage}/activate $@
          '').outPath;
        };
      };
    });

    defaultApp = forAllSystems ({ system, ... }: inputs.self.apps.${system}.delta);

    nixosModules = let
      mergeAll = lib.fold lib.recursiveUpdate {};
      pathsToAttrs = map (file:
        let cleanFile = lib.removeSuffix ".nix" (lib.removePrefix "${toString ./.}/" (toString file));
        in lib.setAttrByPath (lib.splitString "/" cleanFile) (import file)
      );
      nixFilesOf = builtins.filter (lib.hasSuffix ".nix");

      moduleList = (import ./modules/nixos.nix)
        ++ (import ./modules/home-manager.nix);

      profilesList = (lib.filesystem.listFilesRecursive ./profiles)
        ++ (builtins.filter (f: ! builtins.hasAttr (builtins.baseNameOf f) (builtins.readDir ./users))
                            (lib.filesystem.listFilesRecursive ./users));
    in (mergeAll (pathsToAttrs (nixFilesOf moduleList)))
    // (mergeAll (pathsToAttrs (nixFilesOf profilesList)))
    // {
      hosts = forAllSystems ({ pkgs, system, ... }: (let
        usr = {
          tools = import ./lib/utils.nix {
            inherit lib;
          };
          elisp = import ./lib/elisp.nix {
            inherit lib;
            pkgs = channels.lib.legacyPackages.${system};
          };
          dag = let dagLib = import ./lib/dag.nix lib lib;
          in dagLib.dag // { inherit (dagLib) types; };
          units = {
            kilobytes = b: b * 1024;
            megabytes = k: k * 1024;
            gigabytes = m: m * 1024;
          };
          inherit (inputs.self.passthru) secrets;
        };

        modulesFor = hostName: appendModules: let
          specialArgs = {
            inherit usr;
            flake = inputs.self;

            inherit (secrets) hosts domains;

            modules = modules ++ [
              { _module.args = specialArgs; }
            ];
            inherit extraModules;
          };

          # External modules
          inherit (inputs.home.nixosModules) home-manager;
          inherit (inputs.dwarffs.nixosModules) dwarffs;
          inherit (inputs.guix.nixosModules) guix;
          inherit (inputs.construct.nixosModules) matrix-construct;
          inherit (inputs.agenix.nixosModules) age;
          hydra = "${inputs.hydra}/hydra-module.nix";
          apparmor-nix = inputs.apparmor.nixosModule;

          # Some common basic stuff
          core = ./profiles/core.nix;

          # The flake-ier common basic stuff
          global = {
            environment.pathsToLink = [ "/share/bios" ];

            documentation.nixos.extraModuleSources = [./.]
              ++ lib.mapAttrsToList (_: x: x.outPath) inputs;

            nix.package = lib.mkDefault pkgs.nixFlakes;
            nix.registry = lib.mapAttrs (id: flake: {
              inherit flake;
              from = { inherit id; type = "indirect"; };
            }) (inputs // { nixpkgs = inputs.master; });
            nix.nixPath = lib.mapAttrsToList (k: v: "${k}=${toString v}") {
              nixpkgs = "${channels.pkgs}/";
              nixos = "${inputs.self}/";
              self = "/run/current-system/flake/input/self/";
              flake = "/srv/git/github.com/bqv/nixrc";
            };

            system.configurationRevision = inputs.self.rev or "dirty";
            system.nixos.versionSuffix = let inherit (inputs) self;
              date = lib.substring 0 8 (self.lastModifiedDate or "19700101");
              rev = self.shortRev or "dirty";
            in lib.mkForce ".${date}.${rev}";

            system.extraSystemBuilderCmds = (''

              mkdir -p $out/flake/input

              # Link first-class inputs
              ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value}' "$out/flake/input/${name}"
              '') inputMap.n1}

              # Link second-class inputs
              ${(lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value}' "$out/flake/input/${name}"
              '') inputMap.n2)}

              # Link third-class inputs (skipped)
              ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value}' "$out/flake/input/${name}"
              '') inputMap.n3}

            '');

            system.activationScripts.etcnixos = ''
              rm -f /etc/nixos && \
              ln -sfn /run/current-system/flake/input/self /etc/nixos || \
              true
            '';
          };

          # Host-specific basic stuff
          host = {
            environment.etc."machine-id".text = builtins.hashString "md5" hostName;

            networking = { inherit hostName; };
          };

          # Nixos eval settings
          nixpkgs = { config, ... }: {
            config.nixpkgs = {
              inherit pkgs;
              system = config.platform;
            };
          };

          # Amend home-manager (inject modules, set common stuff)
          home = { config, ... }: {
            options.home-manager.users = lib.mkOption {
              type = with lib.types; attrsOf (submoduleWith {
                inherit specialArgs;
                modules = let
                  flakeModules = import ./modules/home-manager.nix;
                  nixProfile = { lib, ... }: {
                    home.activation.disableNixEnv = lib.hm.dag.entryBefore ["installPackages"] ''
                      alias nix-env=true
                    '';
                    home.activation.installPackages = lib.mapAttrs (k: v: lib.mkForce v) (lib.hm.dag.entryAnywhere "true");
                  };
                  baduk = {
                    imports = [ (import inputs.baduk) ];
                    baduk.sabaki.engines = lib.mkDefault [];
                  };
                  impermanence = import "${inputs.impermanence}/home-manager.nix";
                in flakeModules ++ [
                  nixProfile
                  baduk
                ];
              });
            };

            config.home-manager = {
              useUserPackages = true;
              useGlobalPkgs = true;
              verbose = true;
            };
          };

          # Hack in the gnupg secrets module (fix docbook)
          gnupg = import "${inputs.pr93659}/nixos/modules/security/gnupg.nix";

          # Plug in the impermanence module (not a flake :<)
          impermanence = import "${inputs.impermanence}/nixos.nix";

          # Set up any other pull request modules
          iwd = { config, ... }: let
            iwdModule = "services/networking/iwd.nix";
          in {
            disabledModules = [ iwdModule ];
            imports = [
              (import "${inputs.pr75800}/nixos/modules/${iwdModule}" {
                inherit config pkgs;
                lib = let
                  iwdLib = import "${inputs.pr75800}/lib/default.nix";
                in lib // {
                  types = {
                    inherit (iwdLib.types) fixedLengthString lengthCheckedString;
                  } // lib.types;
                };
              })
            ];
          };

          # Our modules
          flakeModules = import ./modules/nixos.nix;

          # Actual host config
          configuration = import "${toString ./hosts}/${hostName}";

          # Modules to propagate to containers
          extraModules = [
            core global
          ];

          # Final modules set
          modules = flakeModules ++ extraModules ++ [
            home nixpkgs iwd gnupg
            home-manager dwarffs matrix-construct hydra
            impermanence age guix apparmor-nix
          ];
        in {
          inherit system specialArgs;
          modules = modules ++ [
            host configuration
          ] ++ appendModules;
        };

        forEachHost = do: usr.tools.recImport {
          # Build a nixos system for each dir in ./hosts using modulesFor
          dir = ./hosts;
          _import = do;
        };
      in forEachHost (host: let
        pkgs = channels.modules.legacyPackages.${system};
      in modulesFor host [])));
    };

    devShell = forAllSystems ({ system, ... }:
      let
        pkgs = import channels.pkgs { inherit system; overlays = [ inputs.devshell.overlay ]; };
      in pkgs.mkDevShell {
        packages = with pkgs; let
          git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
            worktreePatch = fetchurl {
              name = "support-worktree-simple-version.patch";
              url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
              sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
              # date = 2021-01-29T23:45:21+0000;
            };
            patches = [ worktreePatch ];
          });
        in [
          git git-crypt git-secrets nixfmt
        ];

        env.NIX_CONF_DIR = with pkgs; let
          nixConf = ''
            ${lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
            experimental-features = nix-command flakes ca-references
            print-build-logs = true
            access-tokens = "github.com=${secrets.git.github.oauth-token}"
          '';
        in linkFarm "nix-conf-dir" ( [
          { name = "nix.conf"; path = writeText "flakes-nix.conf" nixConf; }
          { name = "registry.json"; path = /etc/nix/registry.json; }
          { name = "machines"; path = /etc/nix/machines; }
        ] );

        commands = [{
          name = "forecast";
          category = "automation";
          command = ''
            export ARGS="$*"
            REPO=$(
              nix build --impure --json --no-link '.#lib.forecast' \
              | jq '.[] | .outputs.out' -r \
            )
            git fetch $REPO substrate:substrate
            git branch -f substrate FETCH_HEAD
          '';
        }];

        motd = "";
      }
    );

    passthru = rec {
      inherit inputs channels config allSystems inputMap patchNixpkgs;
      patchedPkgs = forAllSystems ({ system, ... }:
        patchNixpkgs (channels.modules.legacyPackages.${system})
      );

      #$ git config secrets.providers "nix eval --raw .#passthru.textFilter"
      textFilter = with inputs.priv.lib.textFilter { inherit lib; }; lines;
      inherit (inputs.priv.lib) secrets;

      forecast = let
        inherit (channels.pkgs.legacyPackages.${builtins.currentSystem}) pkgs;
        date = builtins.readFile (pkgs.runCommandLocal "forecast-name" {
          nonce = builtins.currentTime;
        } "date -Iminutes | tr 'T\\-:' '---' | cut -d+ -f1 | tr -d '\n' > $out");
      in pkgs.runCommandLocal "forecast-${date}" rec {
        repo = builtins.getEnv "PWD";
        inputs = builtins.getEnv "ARGS";
        buildInputs = [ pkgs.git pkgs.nixUnstable pkgs.cacert ];
        PAGER = "cat";
        GIT_AUTHOR_NAME = "ci";
        GIT_AUTHOR_EMAIL = "nix@system";
        GIT_COMMITTER_NAME = "systemd";
        GIT_COMMITTER_EMAIL = "timer@service";
        __noChroot = true;
      } ''
        git clone $repo $out
        export NIX_REMOTE=local?real=$PWD/nix/store\&store=/nix/store
        export NIX_STATE_DIR=$PWD/nix/var NIX_LOG_DIR=$PWD/nix/var/log
        export HOME=$PWD
        cd $out
        git switch -c substrate live
        git commit-tree -m "merge: live" -p HEAD -p origin/live origin/live:
        git reset --hard origin/live
        for INPUT in $inputs; do
          nix flake update --update-input $INPUT \
            --experimental-features "nix-command flakes ca-references"
          git commit -m "flake(lock): autoupdate $INPUT" flake.lock
        done
      '';
    };

    hydraJobs = rec {
      deployment = forAllSystems ({ system, ... }:
        inputs.self.defaultPackage.${system}
      );
    };
  };
}
#+END_SRC
