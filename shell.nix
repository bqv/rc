{ pkgs ? import <nixpkgs> { } }:
let
  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    FLAKE=$(git rev-parse --show-toplevel)
    source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo $@

    if [ -e "$pathToConfig" ]; then
      echo Tagging $pathToConfig
      git tag $(basename $pathToConfig)
    fi
    if [ -e "$systemNumber" ]; then
      echo Tagging $(hostname)/$systemNumber
      git tag $(hostname)/$systemNumber
    fi
  '';
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ git git-crypt nixFlakes rebuild ];

  shellHook = ''
    mkdir -p secrets
  '';

  NIX_CONF_DIR = let
    current = pkgs.lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
      (builtins.readFile /etc/nix/nix.conf);

    nixConf = pkgs.writeTextDir "opt/nix.conf" ''
      ${current}
      experimental-features = nix-command flakes ca-references
    '';
  in "${nixConf}/opt";
}
