{ config, lib, pkgs, ... }:

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
        colorscheme slate

        set inccommand=nosplit # previews
        set nohlsearch
        set number
      '';
    };
  };
}
