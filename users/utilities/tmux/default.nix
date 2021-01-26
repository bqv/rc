{ config, pkgs, lib, ... }:

{
  config.programs.tmux = lib.mkIf config.programs.tmux.enable {
    aggressiveResize = true;
    escapeTime = 10;
    extraConfig = with builtins; ''
      ${readFile ../../../profiles/develop/tmux/tmuxline.conf}

      ${readFile ../../../profiles/develop/tmux/tmux.conf}
    '';
    historyLimit = 5000;
    keyMode = "vi";
    shortcut = "q";
    baseIndex = 1;
    plugins = with pkgs.tmuxPlugins; [
      copycat
      open
      resurrect
      yank
      vim-tmux-navigator
    ];
  };
}
