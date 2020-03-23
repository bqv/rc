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
      gnupg bitwarden-cli protonvpn-cli-ng # Security
      file jq # Utility
      netsurf.browser # Utility
    ];

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

    programs.fish.enable = true;
    programs.fish.promptInit = ''
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
    programs.fish.interactiveShellInit = ''
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

alias tmux 'tmux -2'	# Force 256 colors
alias less "less -R"
alias diff "diff -s"
alias bw 'env (cat ~/.bwrc) bw'

alias nix-build "nix-build --no-out-link"

alias sstart "sudo systemctl start"
alias sstop "sudo systemctl stop"
alias srestart "sudo systemctl restart"
alias sstatus "sudo systemctl status"
alias senable "sudo systemctl enable"
alias sdisable "sudo systemctl disable"
alias smask "sudo systemctl mask"
alias sunmask "sudo systemctl unmask"
alias sreload "sudo systemctl daemon-reload"

alias ustart "systemctl start --user"
alias ustop "systemctl stop --user"
alias urestart "systemctl restart --user"
alias ustatus "systemctl status --user"
alias uenable "systemctl enable --user"
alias udisable "systemctl disable --user"

function exwm-exec
    emacsclient --eval '(bqv/exwm-exec "'"$argv"'")'
end
function exwm-sudo-exec
    emacsclient --eval '(bqv/exwm-sudo-exec "'"$argv"'")'
end
function find-file
    emacsclient --eval '(find-file "'"$argv"'")'
end

function please		# Sudo last command
    eval sudo $history[1]
end

fortune -as linux linuxcookie paradoxum computers science definitions | tee -a /tmp/fortune.log | cowsay
echo -e '\n' >> /tmp/fortune.log
    '';
  };
}
