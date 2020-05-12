{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.neovim;
in {
  config = mkIf cfg.enable rec {
    programs.neovim = {
      viAlias = true;
      vimAlias = true;
      withPython3 = true;
      withRuby = true;
      withNodeJs = true;
      plugins = with pkgs.vimPlugins; [
        vim-nix # nix language
      ];
      extraConfig = ''
        set inccommand=nosplit
        set nohlsearch
        set number
        colorscheme slate
      '';
    };
  };
}
