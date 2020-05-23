{ pkgs }:

pkgs.writeShellScriptBin "nixFlakes-shell" ''
  nix-shell -E 'with import "${pkgs.path}/nixos" { configuration.nix.package = (import <nixpkgs> {}).nixFlakes; }; pkgs.mkShell { buildInputs = with config.system.build; with pkgs; [ nixos-rebuild ]; }' $@
''
