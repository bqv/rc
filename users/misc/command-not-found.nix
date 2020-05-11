{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.command-not-found;
in {
  programs.command-not-found.dbPath = pkgs.runCommandNoCC "programs.sqlite" {} ''
    cat ${pkgs.nixexprs}/nixos*/programs.sqlite > $out
  '';
}
