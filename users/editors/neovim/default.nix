{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.neovim;
in {
  config = mkIf cfg.enable rec {
    programs.neovim = {
      viAlias = true;
      vimAlias = true;
      withNodeJs = true;
    };
  };
}
