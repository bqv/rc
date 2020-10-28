{ config, pkgs, lib, flakes, ... }:

{
  imports = [
    ./shells/fish
    ./shells/xonsh
    ./browsers/nyxt
    ./editors/vim
   #./media/radio
    ./media/mpv
   #./utilities/ssh
    ./utilities/git
    ./utilities/darcs
    ./utilities/tmux
    ./utilities/htop
    ./services/gnupg
  ];

  home.file.".bashrc".text = ''
    # If not running interactively, don't do anything
    [[ $- != *i* ]] && return

    . /etc/profile
    . $HOME/.bash_profile

    [[ -x "$HOME/.nix-profile/bin/fish" ]] && exec $HOME/.nix-profile/bin/fish

    PS1='[\u@\h \W]\$ '
  '';
  home.file.".bash_profile".text = ''
    . $HOME/.nix-profile/etc/profile.d/nix.sh

    [[ $- == *i* ]] && \
    [[ -x "$HOME/.nix-profile/bin/fish" ]] && exec $HOME/.nix-profile/bin/fish
  '';
  home.file.".config/nixpkgs/config.nix".text = ''
    {
      ${lib.optionalString config.nixpkgs.config.allowUnfree "allowUnfree = true;"}
    }
  '';

  programs.home-manager.enable = true;
  programs.command-not-found.enable = false;
  programs.xonsh.enable = true;
  programs.fish.enable = true;
  programs.htop.enable = true;
  programs.bat.enable = true;
  programs.fzf.enable = true;
  programs.tmux.enable = true;
  programs.neovim.enable = true;
  programs.jq.enable = true;
  programs.direnv.enable = true;
  programs.mpv.enable = true;
  programs.feh.enable = true;
  programs.git.enable = true;
  programs.ssh.enable = true;

  services.lorri.enable = true;
  services.gpg-agent.enable = true;
  services.unclutter.enable = true;
  services.unclutter.extraOptions = [ "ignore-scrolling" ];
  services.sxhkd.enable = true;
  services.sxhkd.extraPath = "/bin:/usr/bin:/usr/local/bin:/snap/bin";
  services.sxhkd.keybindings = let
    mod = "super";
  in {
    ## wm independent hotkeys

    # terminal emulator
    "${mod} + {_,shift + }Return" = "st -f 'DejaVu Sans Mono-9' {env -u TMUX_TMPDIR $SHELL, -- mosh bao@delta -- emacsclient -t}";

    # program launcher
    "${mod} + @space" = "rofi -show run";

    # make sxhkd reload its configuration files:
    "${mod} + Escape" = "pkill -USR1 -x sxhkd";

    ## bspwm hotkeys

    # quit bspwm normally
    "${mod} + alt + Escape" = "bspc quit";

    # close and kill
    "${mod} + {_,shift + }w" = "bspc node -{c,k}";

    # alternate between the tiled and monocle layout
    "${mod} + m" = "bspc desktop -l next";

    # if the current node is automatic, send it to the last manual, otherwise pull the last leaf
    "${mod} + y" = "bspc query -N -n focused.automatic && bspc node -n last.!automatic || bspc node last.leaf -n focused";

    # swap the current node and the biggest node
    "${mod} + g" = "bspc node -s biggest";

    ## state/flags

    # set the window state
    "${mod} + {t,shift + t,s,f}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";

    # set the node flags
    "${mod} + ctrl + {x,y,z}" = "bspc node -g {locked,sticky,private}";

    ## focus/swap

    # focus the node in the given direction
    "${mod} + {_,shift + }{h,j,k,l}" = "bspc node -{f,s} {west,south,north,east}";

    # focus the node for the given path jump
    "${mod} + {p,b,comma,period}" = "bspc node -f @{parent,brother,first,second}";

    # focus the next/previous node in the current desktop
    "${mod} + {_,shift + }c" = "bspc node -f {next,prev}.local";

    # focus the next/previous desktop in the current monitor
    "${mod} + bracket{left,right}" = "bspc desktop -f {prev,next}.local";

    # focus the last node/desktop
    "${mod} + {grave,Tab}" = "bspc {node,desktop} -f last";

    # focus the older or newer node in the focus history
    "${mod} + {o,i}" = ''bspc wm -h off; \
        bspc node {older,newer} -f; \
        bspc wm -h on'';

    # focus or send to the given desktop
    "${mod} + {_,shift + }{1-9,0}" = "bspc {desktop -f,node -d} '^{1-9,10}'";

    ## preselect

    # preselect the direction
    "${mod} + ctrl + {h,j,k,l}" = "bspc node -p {west,south,north,east}";

    # preselect the ratio
    "${mod} + ctrl + {1-9}" = "bspc node -o 0.{1-9}";

    # cancel the preselection for the focused node
    "${mod} + ctrl + space" = "bspc node -p cancel";

    # cancel the preselection for the focused desktop
    "${mod} + ctrl + shift + space" = "bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel";

    ## move/resize

    # expand a window by moving one of its side outward
    "${mod} + alt + {h,j,k,l}" = "bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}";

    # contract a window by moving one of its side inward
    "${mod} + alt + shift + {h,j,k,l}" = "bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}";

    # move a floating window
    "${mod} + {Left,Down,Up,Right}" = "bspc node -v {-20 0,0 20,0 -20,20 0}";
  };

  #systemd.user.startServices = true; # broken by the [nix-env -> nix profile] move

  xsession.windowManager.bspwm.enable = true;
  xdg.configFile."bspwm/bspwmrc".executable = true;
  xdg.configFile."bspwm/bspwmrc".text = let
    compiledLayout = with pkgs.xorg; pkgs.runCommand "xkb-layout" {} ''
      ${xkbcomp}/bin/xkbcomp -I${./data/xkb} ${./data/xkb/keymap/cb} $out
    '';
  in ''
    #!/bin/sh

    xkbcomp ${compiledLayout} $DISPLAY
    sxhkd &
    unclutter &

    bspc monitor -d I II III IV V

    bspc config border_width         2
    bspc config window_gap          12

    bspc config split_ratio          0.52
    bspc config borderless_monocle   true
    bspc config gapless_monocle      true

    bspc rule -a Gimp desktop='^8' state=floating follow=on
    bspc rule -a qutebrowser desktop='^2'
    bspc rule -a mplayer2 state=floating
    bspc rule -a Kupfer.py focus=on
    bspc rule -a Screenkey manage=off
  '';
  home.file.".Xmodmap".text = ''
    remove Mod4 = Super_L
    remove Control = Control_L
    keysym Control_L = Super_L
    keysym Super_L = Control_L
    add Mod4 = Super_L
    add Control = Control_L
  '';

  xdg.configFile."nix/registry.json".text = builtins.toJSON {
    flakes = [
      {
        from = {
          id = "rc";
          type = "indirect";
        };
        to = {
          owner = "bqv";
          repo = "nixrc";
          type = "github";
        };
      }
      {
        from = {
          id = "self";
          type = "indirect";
        };
        to = let
          flake = flakes.self;
        in {
          inherit (flake) narHash lastModified;
          path = flake.outPath;
          rev = if flake ? rev then flake.rev
                else "0000000000000000000000000000000000000000";
          type = "path";
        };
      }
      {
        from = {
          id = "nixpkgs";
          type = "indirect";
        };
        to = let
          flake = flakes.rel2009;
        in {
          inherit (flake) narHash lastModified;
          path = flake.outPath;
          rev = if flake ? rev then flake.rev
                else "0000000000000000000000000000000000000000";
          type = "path";
        };
      }
    ];
    version = 2;
  };
}
