{ config ? {}, lib, pkgs, ... }:

let
  myEmacs = pkgs.emacsGit.overrideAttrs (attrs: {
    withXwidgets = true;
    webkitgtk = pkgs.webkitgtk;
  });
in rec {
  imports = [
    ./packages.nix
  ];

  environment.systemPackages = with pkgs; [
    config.services.emacs.package
    nixfmt
  ];

  services.emacs.package = let
    daemonScript = pkgs.writeScript "emacs-wrapper" ''
      #!/usr/bin/env bash
      if [[ "$@" =~ "--fg-daemon" ]] || [[ "$@" =~ "--daemon" ]]; then
        echo Redirecting output to journal tag: emacs
        systemd-cat -t emacs ${myEmacs}/bin/emacs ''${@//--daemon/--fg-daemon} & disown
        while [[ "$(emacsclient --eval 'init-done' 2>&1)" != "t" ]]; do
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

  fonts.fonts = [
    pkgs.emacs-all-the-icons-fonts
  ];
  services.emacs = {
    enable = true;
    install = true;
  };

  # Enable the X11 windowing system.
  services.xserver = let systemEmacs = myEmacs; in {
    displayManager = with pkgs; {
      defaultSession = "none+emacsclient";
      session = [{
        manage = "window";
        name = "emacsclient";
        start = ''
          ${systemd}/bin/systemctl --user start emacs || exit 1
          ${systemEmacs}/bin/emacsclient -a "" -c &
          waitPID=$!
        '';
      }];
      sessionCommands = ''
       echo Executing: $0 $@
       #${wmname}/bin/wmname LG3D

       #${xorg.xsetroot}/bin/xsetroot -cursor-name left_ptr # fallback cursor
       #${xorg.xrdb}/bin/xrdb -merge ~/.Xresources
       #${xorg.xrdb}/bin/xrdb -merge ~/.Xdefaults
       #${xorg.xset}/bin/xset -dpms
       #${xorg.xset}/bin/xset s off
       #${xorg.xset}/bin/xset m 64 0
       #${xorg.xhost}/bin/xhost +
       ${ibus}/bin/ibus-daemon -drRx

       #export VISUAL=${systemEmacs}/bin/emacsclient

       #exec ${systemEmacs}/bin/emacsclient -a "" -c
      '';
      sddm = {
        enable = true;
        autoLogin = {
          enable = true;
          relogin = true;
          user = "bao";
        };
      };
    };
  };
}
