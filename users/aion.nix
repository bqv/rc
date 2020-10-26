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

    source /etc/profile

    export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive

    PS1='[\u@\h \W]\$ '

    PS1="\n\[\e[1;30m\][''$$:$PPID - \j:\!\[\e[1;30m\]]\[\e[0;36m\] \T \[\e[1;30m\][\[\e[1;34m\]\u@\H\[\e[1;30m\]:\[\e[0;37m\]''${SSH_TTY:-o} \[\e[0;32m\]+''${SHLVL}\[\e[1;30m\]] \[\e[1;37m\]\w\[\e[0;37m\] \n\$ "
  '';
  home.file.".profile".text = ''
  '';
  home.file.".bash_profile".text = ''
    export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
    [[ $INSIDE_EMACS == "vterm" ]] && [[ $IN_NIX_SHELL == "" ]] && exec fish
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
  services.sxhkd.keybindings = {
    ## wm independent hotkeys

    # terminal emulator
    "mode_switch + {_,shift + }Return" = "stterm -f 'DejaVu Sans Mono-9'{_, -- mosh bao@delta -- emacsclient -t}";

    # program launcher
    "mode_switch + @space" = "rofi -show run";

    # make sxhkd reload its configuration files:
    "mode_switch + Escape" = "pkill -USR1 -x sxhkd";

    ## bspwm hotkeys

    # quit bspwm normally
    "mode_switch + alt + Escape" = "bspc quit";

    # close and kill
    "mode_switch + {_,shift + }w" = "bspc node -{c,k}";

    # alternate between the tiled and monocle layout
    "mode_switch + m" = "bspc desktop -l next";

    # if the current node is automatic, send it to the last manual, otherwise pull the last leaf
    "mode_switch + y" = "bspc query -N -n focused.automatic && bspc node -n last.!automatic || bspc node last.leaf -n focused";

    # swap the current node and the biggest node
    "mode_switch + g" = "bspc node -s biggest";

    ## state/flags

    # set the window state
    "mode_switch + {t,shift + t,s,f}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";

    # set the node flags
    "mode_switch + ctrl + {x,y,z}" = "bspc node -g {locked,sticky,private}";

    ## focus/swap

    # focus the node in the given direction
    "mode_switch + {_,shift + }{h,j,k,l}" = "bspc node -{f,s} {west,south,north,east}";

    # focus the node for the given path jump
    "mode_switch + {p,b,comma,period}" = "bspc node -f @{parent,brother,first,second}";

    # focus the next/previous node in the current desktop
    "mode_switch + {_,shift + }c" = "bspc node -f {next,prev}.local";

    # focus the next/previous desktop in the current monitor
    "mode_switch + bracket{left,right}" = "bspc desktop -f {prev,next}.local";

    # focus the last node/desktop
    "mode_switch + {grave,Tab}" = "bspc {node,desktop} -f last";

    # focus the older or newer node in the focus history
    "mode_switch + {o,i}" = ''bspc wm -h off; \
        bspc node {older,newer} -f; \
        bspc wm -h on'';

    # focus or send to the given desktop
    "mode_switch + {_,shift + }{1-9,0}" = "bspc {desktop -f,node -d} '^{1-9,10}'";

    ## preselect

    # preselect the direction
    "mode_switch + ctrl + {h,j,k,l}" = "bspc node -p {west,south,north,east}";

    # preselect the ratio
    "mode_switch + ctrl + {1-9}" = "bspc node -o 0.{1-9}";

    # cancel the preselection for the focused node
    "mode_switch + ctrl + space" = "bspc node -p cancel";

    # cancel the preselection for the focused desktop
    "mode_switch + ctrl + shift + space" = "bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel";

    ## move/resize

    # expand a window by moving one of its side outward
    "mode_switch + alt + {h,j,k,l}" = "bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}";

    # contract a window by moving one of its side inward
    "mode_switch + alt + shift + {h,j,k,l}" = "bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}";

    # move a floating window
    "mode_switch + {Left,Down,Up,Right}" = "bspc node -v {-20 0,0 20,0 -20,20 0}";
  };

  #systemd.user.startServices = true; # broken by the [nix-env -> nix profile] move

  xsession.windowManager.bspwm.enable = true;
  xdg.configFile."bspwm/bspwmrc".executable = true;
  xdg.configFile."bspwm/bspwmrc".text = ''
    #!/bin/sh

    systemctl --user start sxhkd
    systemctl --user start unclutter

    bspc monitor -d I II III IV V

    bspc config border_width         2
    bspc config window_gap          12

    bspc config split_ratio          0.52
    bspc config borderless_monocle   true
    bspc config gapless_monocle      true

    bspc rule -a Gimp desktop='^8' state=floating follow=on
    bspc rule -a Chromium desktop='^2'
    bspc rule -a mplayer2 state=floating
    bspc rule -a Kupfer.py focus=on
    bspc rule -a Screenkey manage=off

    #echo | xmodmap - <<END
    #  remove Mod4 = Super_L
    #  remove Control = Control_L
    #  keysym Control_L = Super_L
    #  keysym Super_L = Control_L
    #  add Mod4 = Super_L
    #  add Control = Control_L
    #END
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
        to = {
          inherit (flakes.self) narHash lastModified;
          path = flakes.self.outPath;
          rev = if flakes.self ? rev then flakes.self.rev
                else "0000000000000000000000000000000000000000";
          type = "path";
        };
      }
    ];
    version = 2;
  };
}
