{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.cargo = {
    demand = true;
    after = [ "rust-mode" ];
    hook = [
      { rust-mode-hook = "cargo-minor-mode"; }
    ];
  };
}
