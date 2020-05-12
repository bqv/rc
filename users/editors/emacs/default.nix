{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.emacs;

  myEmacs = (pkgs.emacs.override {
    srcRepo = true;
    withXwidgets = true;
    webkitgtk = pkgs.large.webkitgtk;
  }).overrideAttrs (o: rec {
    name = "gccemacs-${lib.substring 0 8 src.rev}";
    src = pkgs.fetchFromGitHub {
      owner = "emacs-mirror";
      repo = "emacs";
      # Fetching off the feature/native-comp git branch
      rev = "92dc81f85e1b91db04487ccf1b52c0cd3328dfee";
      sha256 = "1f22bxwq53hhdjlakmqz66y63vix5ybpnc1pk9fpy18wjh871scq";
    };
    patches = [];
    postPatch = ''
      substituteInPlace lisp/loadup.el \
        --replace '(emacs-repository-get-version)' '"${src.rev}"' \
        --replace '(emacs-repository-get-branch)' '"feature/native-comp"'
    '';

    # When this is enabled, emacs does native compilation lazily after starting
    # up, resulting in quicker package builds up-front, at the cost of slower
    # running emacs until everything has been compiled.
    #makeFlags = [ "NATIVE_FAST_BOOT=1" ];

    LIBRARY_PATH = "${lib.getLib pkgs.stdenv.cc.libc}/lib";

    configureFlags = o.configureFlags ++ [ "--with-nativecomp" ];

    buildInputs = with pkgs; o.buildInputs ++ [ jansson libgccjit glibc harfbuzz.dev ];
  });
in {
  imports = [
    ./packages.nix
  ];

  config = mkIf cfg.enable rec {
    home.packages = with pkgs; [
      pkgs.emacs-all-the-icons-fonts
      nixfmt
    ];

    programs.emacs.package = let
      daemonScript = pkgs.writeScript "emacs-wrapper" ''
        #!${pkgs.runtimeShell}
        if [[ "$@" =~ "--fg-daemon" ]] || [[ "$@" =~ "--daemon" ]]; then
          echo Redirecting output to journal tag: emacs
          systemd-cat -t emacs ${myEmacs}/bin/emacs ''${@//--daemon/--fg-daemon} & disown
          while [[ "$(${myEmacs}/bin/emacsclient --eval 'init-done' 2>&1)" != "t" ]]; do
            sleep 1
          done
        else
          exec ${myEmacs}/bin/emacs $@
        fi
      '';
    in pkgs.stdenv.mkDerivation {
      name = "${myEmacs.name}-wrapped";
      passthru = { unwrapped = myEmacs; };
      buildInputs = with pkgs; [ makeWrapper ];
      phases = [ "installPhase" ];
      installPhase = ''
        mkdir -p $out/bin/
        makeWrapper ${daemonScript} $out/bin/emacs \
          --argv0 emacs
        cp -rus ${myEmacs}/* $out/
        rm -f $out/bin/emacs-w64
      '';
    };

    #fonts.fontconfig.enable = true;
  };
}
