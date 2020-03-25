{ pkgs, ... }: {
  imports = [ ../../profiles/develop ];

  users.users.bao = {
    uid = 1000;
    description = "default";
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" "adbusers" ];
    packages = with pkgs; [
      texmaker texlive.combined.scheme-full
    ];
  } // import ../../secrets/user.password.nix;

  home-manager.users.bao = {
    programs.home-manager.enable = true;

    home.packages = with pkgs; [
      abduco dvtm git yadm vim htop pstree fortune cowsay coreutils pv # Shell Essential
      nmap wget curl # Networking
      gnupg pinentry bitwarden-cli protonvpn-cli-ng git-crypt # Security
      file jq direnv # Utility
      netsurf.browser # Utility
    ];

   #xdg = {
   #  enable = true;
   #  mimeApps = {
   #    enable = true;
   #  };
   #};

    services.lorri = {
      enable = true;
    };

   #services.gpg-agent = {
   #  enable = true;
   #  defaultCacheTtl = 600;
   #  defaultCacheTtlSsh = 0;
   #  maxCacheTtl = 7200;
   #  enableSshSupport = true;
   #  sshKeys = [ "C425D701DBB41091CAC74AB2A7476FC5237EDBC7" ];
   #  extraConfig = ''
   #    allow-emacs-pinentry
   #    allow-preset-passphrase
   #    pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
   #  '';
   #};

    programs.fish = {
      enable = true;
      promptInit = ''
        set fish_prompt_pwd_dir_length 1
        set __fish_git_prompt_show_informative_status 1
        
        # Fish command and parameter colors
        set fish_color_command green
        set fish_color_param $fish_color_normal
        
        # Git prompt
        set __fish_git_prompt_showdirtystate 'yes'
        set __fish_git_prompt_showupstream 'yes'
        
        set __fish_git_prompt_color_branch brown
        set __fish_git_prompt_color_dirtystate FCBC47
        set __fish_git_prompt_color_stagedstate yellow
        set __fish_git_prompt_color_upstream cyan
        set __fish_git_prompt_color_cleanstate green
        set __fish_git_prompt_color_invalidstate red
        
        # Git Characters
        set __fish_git_prompt_char_dirtystate '*'
        set __fish_git_prompt_char_stateseparator ' '
        set __fish_git_prompt_char_untrackedfiles ' …'
        set __fish_git_prompt_char_cleanstate '✓'
        set __fish_git_prompt_char_stagedstate '⇢ '
        set __fish_git_prompt_char_conflictedstate "✕"
        
        set __fish_git_prompt_char_upstream_prefix ' '
        set __fish_git_prompt_char_upstream_equal ' '
        set __fish_git_prompt_char_upstream_ahead '⇡'
        set __fish_git_prompt_char_upstream_behind '⇣'
        set __fish_git_prompt_char_upstream_diverged '⇡⇣'
        
        function _print_in_color
          set -l string $argv[1]
          set -l color  $argv[2]
        
          set_color $color
          printf $string
          set_color normal
        end
        
        function _prompt_color_for_status
          if test $argv[1] -eq 0
            echo magenta
          else
            echo red
          end
        end
        
        function fish_nix_prompt
          if test -n "$IN_NIX_SHELL"
            echo -n "<nix-shell> "
          end
        end
        
        function fish_prompt --description 'Write out the prompt'
            set -l color_cwd
            set -l suffix
            switch "$USER"
                case root toor
                    if set -q fish_color_cwd_root
                        set color_cwd $fish_color_cwd_root
                    else
                        set color_cwd $fish_color_cwd
                    end
                    set suffix '#'
                case '*'
                    set color_cwd $fish_color_cwd
                    set suffix '>'
            end
        
            echo -n -s (fish_nix_prompt) "$USER" @ (prompt_hostname) ' ' (set_color $color_cwd) (prompt_pwd) (set_color normal) "$suffix "
        end
      '';
      interactiveShellInit = ''
        if test -z "$EMACS"
            if test "$TERM" = "dumb"
                exec bash
            else if test -n "$DISPLAY"                # If we're in X11
                test -z "$TMUX"; and exec tmux new -A -s (echo X$DISPLAY | sed 's/X:/X/;s/:/-/')
            else if contains (tty) /dev/tty*          # If we're in TTY
                test -z "$WINDOW"; and exec screen -xRR
            else if test -n "$SSH_CONNECTION"         # If we're in SSH
                if test -n "$MOBILE"                  # If we're on mobile
                    test -z "$DVTM"; and exec env -u MOBILE TERM=dvtm-256color abduco -A  main dvtm -m '^q'
                else
                    test -z "$DVTM"; and exec env           TERM=dvtm-256color abduco -Al main dvtm -m '^q'
                end
            else
                abduco -l
                #read -n
            end
        else
            set -x LC_ALL 'en_GB'
            set -x LANG 'en_GB'
            set -x LC_CTYPE C
            set -x SHELL "emacs $EMACS"
            set -x TERM "emacs $EMACS"
        end
        
        date

        eval (direnv hook fish)
        
        fortune -as linux linuxcookie paradoxum computers science definitions | tee -a /tmp/fortune.log | cowsay
        echo -e '\n' >> /tmp/fortune.log
      '';
        
      shellAliases = {
        tmux = "tmux -2"; # Force 256 colors
        less = "less -R";
        diff = "diff -s";
        bw = "env (cat ~/.bwrc) bw";
        
        nix-build = "nix-build --no-out-link";
        
        sstart = "sudo systemctl start";
        sstop = "sudo systemctl stop";
        srestart = "sudo systemctl restart";
        sstatus = "sudo systemctl status";
        senable = "sudo systemctl enable";
        sdisable = "sudo systemctl disable";
        smask = "sudo systemctl mask";
        sunmask = "sudo systemctl unmask";
        sreload = "sudo systemctl daemon-reload";
        
        ustart = "systemctl start --user";
        ustop = "systemctl stop --user";
        urestart = "systemctl restart --user";
        ustatus = "systemctl status --user";
        uenable = "systemctl enable --user";
        udisable = "systemctl disable --user";
      };
        
      functions = {
        exwm-exec = '' emacsclient --eval '(bqv/exwm-exec "'"$argv"'")' '';
        exwm-sudo-exec = '' emacsclient --eval '(bqv/exwm-sudo-exec "'"$argv"'")' '';
        find-file = '' emacsclient --eval '(find-file "'"$argv"'")' '';
        please = '' eval sudo $history[1] '';
      };

      plugins = [
        {
          name = "bass";
          src = pkgs.fetchFromGitHub {
            owner = "edc";
            repo = "bass";
            rev = "c0d11420f35cfbcb62f94be0dfcf9baf70a9cea5";
            sha256 = "07x4zvm04kra6cc1224mxm6mdl0gggw0ri98kdgysax53cm8r95r";
          };
        }
      ];
    };
  };
}
