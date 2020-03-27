{ config ? {}, lib, pkgs, ... }:

let
  #pkgs.emacsGit missing?
  myEmacs = pkgs.emacs.overrideAttrs (attrs: {
    withXwidgets = true;
    webkitgtk = pkgs.webkitgtk;
  });
in rec {
  imports = [
    ./packages.nix
  ];

  home.packages = with pkgs; [
    pkgs.emacs-all-the-icons-fonts
    nixfmt
  ];

  services.emacs.enable = true;
  programs.emacs.enable = true;
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

  xsession = {
    enable = true;
    windowManager = {
      command = with pkgs; ''
        #${systemd}/bin/systemctl --user start emacs || exit 1
        ${config.programs.emacs.finalPackage}/bin/emacsclient -a "" -c &
        waitPID=$!
      '';
    };
    initExtra = with pkgs; ''
      echo Executing: $0 $@
      ${ibus}/bin/ibus-daemon -drRx
    '';
  };
}
